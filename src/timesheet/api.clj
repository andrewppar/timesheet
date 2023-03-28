(ns timesheet.api
  (:require [compojure.core :as compojure]
            [timesheet.assert :as assert]
            ;;[timesheet.migrate :as migrate]
            ;;[timesheet.serialize_tasks :as serialize]
            [compojure.route :as route]))

(defn introduction
  "A welcome page for anyone who has
  not used this app before"
  [_]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body "Welcome to the task manager!"})

;;(defn ^:private migrate-db
;;  [req]
;;  (migrate/db (get-in req [:params :dpspec])))
;;
;;(defn migrate
;;  "Migrate old database entries to the currently configured one."
;;  [_]
;;  {:status 200
;;   :headers {"Content-Type" "text/json"}
;;   :body (migrate-db req)})

;;(defn -serialize-tasks-by-date
;;  "Serialize the tasks for a date range"
;;  [req]
;;  (let [start (->> req :params :start)
;;        tasks (if-let [end (->> req :params :end)]
;;                (db/parse-dates start end   globals/db-root)
;;                (db/parse-dates start start globals/db-root))]
;;    (serialize/tasks-by-date-and-group tasks)))
;;
;;(defn serialize-tasks-by-date
;;  "A dispatcher for different ways of getting task information"
;;  [req]
;;  {:status 200
;;   :headers {"Content-Type" "text/json"}
;;   :body (-serialize-tasks-by-date req)
;;   })
;;
;;(defn serialize-date-tasks
;;  "A dispatcher to get the tasks for a date"
;;  [req]
;;  {:status 200
;;   :headers {"Content-Type" "text/json"}
;;   :body (->> req
;;              :params
;;              :date
;;              (str globals/db-root "/")
;;              db/parse-task-file
;;              serialize/serialize-date)})

(defn -add-task-from-request
  "Pulls out information from a request
  and adds the corresponding task to the database"
  [req]
  (let [date  (->> req :params :date)
        start (->> req :params :start)
        end   (->> req :params :end)
        group (->> req :params :group)
        desc  (->> req :params :description)]
    (assert/assert! [date start end group desc])))

(defn add-task
  "A dispatcher to add a new task to the database"
  [req]
  {:status 200
   :headers {"Content-Type" "text/json"}
   :body (-add-task-from-request req)})

(defn -delete-task-from-request
  "Pulls out information from a
  request and deletes the corresponding task"
  [req]
  (let [date  (->> req :params :date)
        start (->> req :params :start)
        end   (->> req :params :end)]
    (assert/retract! [date start end])))

(defn delete-task
  "A dispatcher to remove a task from the database"
  [req]
  {:status 200
   :headers {"Content-Type" "text/json"}
   :body (-delete-task-from-request req)})

(compojure/defroutes app-routes
  (compojure/GET "/" [] introduction)
  ;; (compojure/GET "/query" [] query)
;;  (compojure/GET "/tasks-by-date" [] serialize-tasks-by-date)
;;  (compojure/GET "/date" [] serialize-date-tasks)
  (compojure/POST "/add" [] add-task)
  (compojure/POST "/delete" [] delete-task)
  (route/not-found "Error, page not found!"))
