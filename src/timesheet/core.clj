(ns timesheet.core
  (:gen-class)
  (:require [ring.middleware.defaults :refer :all]
            [org.httpkit.server :as server]
            [timesheet.db :as db]
            [timesheet.serialize_tasks :as serialize]
            [timesheet.api :as api]
            ))

(defn -main
  "This is the main point of entry to the app"
  [& args]
  (let [port (Integer/parseInt (or (System/getenv "PORT") "3000"))]
    ;; Run the server with the Ring.defaults middleware
    (server/run-server (wrap-defaults #'api/app-routes site-defaults)
                       {:port port})
    (println (str "Running tasks webserver at localhost:" port "/"))))

(defn get-tasks-main
  ([dates]
   (-main dates true))
  ([dates truncate-descriptions?]
   (let [root  "/Users/andrew/Documents/Records_flat"
         filepaths (map (fn [date] (str root "/" date)) dates)
         tasks     (reduce (fn [acc filepath]
                             (concat acc (db/parse-task-file filepath)))
                           [] filepaths)]
     (serialize/tabular-show-tasks tasks truncate-descriptions?))))

(comment
  (do
    (require '[clojure.tools.namespace.repl :refer [refresh]])
    (refresh))
  )
