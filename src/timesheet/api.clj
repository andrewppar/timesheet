(ns timesheet.api
  (:require [compojure.core :as compojure]
            [timesheet.serialize_tasks :as serialize]
            [timesheet.db :as db]
            [timesheet.globals :as globals]
            [compojure.route :as route]
            [timesheet.time :as time]))

(defn introduction
  "A welcome page for anyone who has
  not used this app before"
  [_]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body "Welcome to the task manager!"})

(defn -serialize-tasks-by-date
  "Serialize the tasks for a date range"
  [req]
  (let [start (->> req
                   :params
                   :start
                   time/new-date-from-string)
        end (if-let [raw-end (->> req
                                  :params
                                  :end)]
              (time/new-date-from-string raw-end)
              start)
        tasks (db/parse-dates start end globals/db-root)]
    (serialize/tasks-by-date-and-group tasks)))

(defn serialize-tasks-by-date
  "A dispatcher for different ways of getting task information"
  [req]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (-serialize-tasks-by-date req) 
   })

(defn serialize-date-tasks
  "A dispatcher to get the tasks for a date"
  [req]
  (->> req
       :params
       :date
       (str globals/db-root "/")
       db/parse-task-file
       serialize/serialize-date))

(compojure/defroutes app-routes
  (compojure/GET "/" [] introduction)
  (compojure/GET "/tasks-by-date" [] serialize-tasks-by-date)
  (compojure/GET "/date" [] serialize-date-tasks)
  (route/not-found "Error, page not found!"))
