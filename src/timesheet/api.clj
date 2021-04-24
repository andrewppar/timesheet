(ns timesheet.api
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [clojure.data.json :as json]))

(defn introduction
  "A welcome page for anyone who has
  not used this app before"
  [_]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body "Welcome to the task manager!"})


(defn serialize-tasks
  "A dispatcher for different ways of getting task information"
  [_]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body "TODO"})

(defroutes app-routes
  (GET "/" [] introduction)
  (GET "/tasks" [] serialize-tasks)
  (route/not-found "Error, page not found!"))
