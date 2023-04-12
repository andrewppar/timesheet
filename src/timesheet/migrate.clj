(ns timesheet.migrate
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [timesheet.assert :as assert]
   [timesheet.legacy-parser :as lp]
   [timesheet.query :as query]
   [timesheet.state :as state]
   [timesheet.taxonomy :as tax]
   [timesheet.time :as time]))


;; TODO: These might be specific to v1 on disk
;; That said, as far as I know, I'm the only one
;; who uses this - so that's all the migration
;; necessary
(defn ^:private task-to-assert-spec
  [date group {:keys [start end description]}]
  [date start end group description])

(defn ^:private task-group-to-assert-specs
  [date {:keys [group tasks]}]
  (map (partial task-to-assert-spec date group) tasks))

(defn task-dates-to-assert-specs
  [{:keys [date task-groups]}]
  (mapcat (partial task-group-to-assert-specs date) task-groups))

(defn migrate-legacy-db
  [directory start end]
  (let [batch 100]
    (loop [current-date start]
      (when-not (time/after? current-date end)
        (let [next-date  (time/plus-days current-date batch)
              task-dates (lp/parse-files
                          directory current-date next-date)
              asserts    (mapcat task-dates-to-assert-specs task-dates)]
          (apply assert/assert! asserts)
          (recur next-date))))))

(defn ^:private location-type
  [location]
  (cond
    (and (string? location)
         (.exists (io/file location)))
    :on-disk))

(defmulti run!
  {:arglists '([db-type location])}
  (fn [db-type location]
    [db-type (location-type location)]))

(defmethod run! [:v1 :on-disk]
  [_ location]
  (migrate-legacy-db
   location "10-11-1987" (time/today)))

(defmethod run! [:v2 :on-disk]
  [_ location]
  ;; TODO: Support this at some point
  ())

(defmethod run! [:v2 :network]
  [_ location]
  ())


(defn earlier-time? [time-one time-two]
  (time/before? time-one time-two))

(defn date-to-legacy-db! [tasks base-directory]
  (let [date   (get (first tasks) :date)
        groups (group-by :group tasks)]
    (spit
     (format "%s/%s" base-directory date)
     (reduce-kv
      (fn [result group tasks]
        (let [sorted-tasks (sort-by :start earlier-time? tasks)
              start        (format "%s{%s\n" result group)
              end          "}\n\n"]
          (str start
               (->> sorted-tasks
                    (mapv
                     (fn [{:keys [start end description]}]
                       (format "%s -- %s * %s" start end description)))
                    (str/join "\n"))
               end)))
      ""
      groups))))

(defn to-legacy-db! [dates base-directory]
  (let [groups        (tax/groups (state/taxonomy))
        sorted-dates  (sort earlier-time? dates)
        tasks         (query/run groups
                        (first sorted-dates) (last sorted-dates) nil)
        grouped-tasks (group-by :date tasks)]
    (pmap
     (fn [date]
       (date-to-legacy-db! (get grouped-tasks date) base-directory))
     dates)))
(comment
  (migrate-legacy-db
   "/Users/andrewparisi/Documents/records/timesheet"
   ["03-01-2019" "07-01-2023"])
  )
