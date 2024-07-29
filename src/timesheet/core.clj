(ns timesheet.core
  (:gen-class)
  (:require [compojure.core           :as compojure]
            [compojure.route          :as route]
            [timesheet.assert         :as assert]
            [timesheet.config         :as config]
            [timesheet.log :refer [defn-logged] :as log]
            [timesheet.json           :as json]
            [timesheet.migrate        :as migrate]
            [timesheet.query          :as query]
            [timesheet.server         :as server]
            [timesheet.state          :as state]
            [timesheet.taxonomy       :as tax]
            [timesheet.time           :as time]
            [clojure.java.io          :as io]))

;; Initialize

(defn ^:private setup!
  "Set up a timesheet service. NOTE: This should only
  be called once - the first time this is used."
  []
  (config/init!)
  (tax/insert-taxonomy!))

(defn init!
  "Set all configuration and start all state for the system."
  []
  (config/init!)
  (log/init!)
  (when-not (state/setup?)
    (setup!))
  (state/init!))

;; Query

(defn all-groups
  "Return all the known groups"
  []
  (tax/groups (state/taxonomy)))

(defn subgroups
  "Given a list of `groups` return all of the
  subgroups for that group"
  [groups]
  (let [taxonomy (state/taxonomy)]
    (reduce
     (fn [subgroup-set group]
       (apply conj subgroup-set (tax/subgroups taxonomy group)))
     #{}
     groups)))

(defn ^:private drop-task?
  [start-date end-date start-time end-time {:keys [date start end]}]
  (or
   (and (= date start-date)
        (or
         (time/before? end start-time)
         (= end start-time)))
   (and (= date end-date)
        (or
         (= start end-time)
         (time/after? start end-time)))))

(defn-logged query
  "Run a query against the database.

  All arguments are optional. The default behavior if no arguments
  are specified is to get all the entries in the database.
  Specifically:
    1. `:groups` - The groups to query using the taxonomy to
           get results from appropriate subgroups. The default is
           to use all groups in the database
    2. `:start-date` - The date to start the search on. The deafult
        is an arbitrary date long enough ago - `10-11-1987`
    3. `:start-time` - The time to start the search on. The default
        time is `00:00`
    4. `:end-date` - The last date to return results from. The default
        date is `:today`.
    5. `:end-time` - The last time to return results from. The default
        time is `23:59`.
    6. `:description` - The text to match on. If no text is specified
         then all text is matched."
  {:level :debug
   :result-fn count}
  [& {:keys
      [groups start-date start-time end-date end-time description] :or
      {groups :all
       start-date "10-11-1987"
       start-time "00:00"
       end-date   :today
       end-time   "23:59"
       }}]

  (let [adjusted-start  (if (= start-date :today) (time/today) start-date)
        adjusted-end    (if (= end-date :today) (time/today) end-date)
        adjusted-groups (into [] (if (= groups :all)
                                   (all-groups)
                                   (subgroups groups)))]
    ;; TODO: Maybe this and the above line should be part of a transducer
    (remove
     (partial drop-task? adjusted-start adjusted-end start-time end-time)
     (query/run adjusted-groups adjusted-start adjusted-end description))))

;;; Migrate

(defn-logged migrate!
  "Migrate an old timesheet database at `db-location` to the current
   version of the database."
  {:level :info}
  [db-location migration-type]
  (migrate/run! migration-type db-location))


;;; Routes

(defn groups
  [_]
  {:status 200
   :headers {"Content-Type" "text/json"}
   :body (json/encode (all-groups))})

(defn status
  "A Status Page"
  [_]
  {:status 200
   :headers {"Content-Type" "text/json"}
   :body    (json/encode {:status :ok})})

(defn ^:private decode-body
  "Decode the body portion of a request."
  [request]
  (with-open [reader
              (io/reader
               (:body request) :encoding "UTF-8")]
    (json/decode (reduce str "" (line-seq reader)))))

(defn ^:private valid-body?
  [body fields]
  (every? (fn [field] (get body field)) fields))

(defn-logged search
  "Search the database."
  {:level :info
   :result-fn count}
  [request]
  (let [body (decode-body request)]
    {:status 200
     :headers {"Content-Type" "text/json"}
     :body (let [{:keys [start_date start_time
                         end_date end_time
                         groups description]} body]
             (cond-> {}
               start_date  (assoc :start-date start_date)
               start_time  (assoc :start-time start_time)
               end_date    (assoc :end-date end_date)
               end_time    (assoc :end-time end_time)
               groups      (assoc :groups   groups)
               description (assoc :description description)
               true       query
               true       json/encode))}))



;; TODO: This is a mess of a function
;; maybe clean it up a little
(defn ^:private create-or-delete-task
  [request fields action-fn]
  (let [body           (decode-body request)
        valid-data?    (valid-body? body fields)
        unknown-group? (and (= action-fn assert/assert!)
                            (not (.contains
                                  (all-groups) (get body :group))))]
    (cond (not valid-data?) {:status 400
                             :headers {"Content-Type" "text/json"}
                             :body {:error (format
                                            "Missing required data: %s"
                                            (into
                                             [] (filter
                                                 #(not (get body %))
                                                 fields)))}}
          unknown-group? {:status 400
                          :headers {"Content-Type" "text/json"}
                          :body {:error (format
                                         "Unknown group: %s"
                                         (get body :group))}}
          :else (let [result (try
                               (action-fn
                                (-> body
                                    (select-keys fields)
                                    vals))
                               {:success true}
                               (catch Exception e {:success false
                                                   :error e}))]
                  {:status (if (get result :success) 201 500)
                   :headers {"Content-Type" "text/json"}
                   :body (json/encode (into {} result))}))))

(defn-logged add-task
    "Given a request containing a `date`, `start_time`,
  `end_time`, `group`, and `description` create a new
  entry in the database for the corresponding task."
   {:level :info
    :result-fn count}
  [request]
  (create-or-delete-task
   request
   [:date :start_time :end_time :group :description]
   assert/assert!))

(defn-logged delete-task
  "Remove a task from teh database.

  NOTE: The identity criteria for a task are completely
  temporal. No two tasks can be worked on with the same start and end."
  {:level :info
   :result-fn count}
  [request]
  (create-or-delete-task
   request [:date :start_time :end_time] assert/retract!))

(defn-logged backup
  "Backup the `dates` to the traditional plain text storage form
   of the database."
  {:level :info
   :result-fn count}
  [request]
  (let [body     (decode-body request)
        dates    (get body :dates)
        base-dir (get body :base_directory)]
    (migrate/to-legacy-db! dates base-dir)
    {:status 200
     :headers {"Content-Type" "text/json"}
     :body (json/encode {:success true})}))

(defn not-found
  "The route doesn't exist"
  [_]
  {:status 404
   :headers {"Content-Type" "text/json"}
   :body {:status "Route does not exist."}})

(compojure/defroutes routes
  (compojure/GET "/groups" [] #'groups)
  (compojure/GET "/status" [] status)
  (compojure/POST "/add"    [] #'add-task)
  (compojure/POST "/delete" [] #'delete-task)
  (compojure/POST "/search" [] #'search)
  (compojure/POST "/backup" [] #'backup)
  (route/not-found not-found))

(defn -main
  [& _]
  (let [port 3000]
    (init!)
    (server/start! routes port)))

(comment
  (-main)
  (query :end-date "03-19-2023" :start-date "03-19-2023")
  )
