(ns timesheet.core
  (:gen-class)
  (:require [ring.middleware.defaults :as middleware]
            [org.httpkit.server :as server]
            [timesheet.db :as db]
            [timesheet.serialize_tasks :as serialize]
            [timesheet.api :as api]
            ))

(defonce server (atom []))


(defn stop-server []
  (when-not (nil? @server)
    ;; graceful shutdown: wait 100ms for existing requests to be finished
    ;; :timeout is optional, when no timeout, stop immediately
    (@server :timeout 100)
    (println "Stopping tasks webserver")
    (reset! server nil)))

(defn -main
  "This is the main point of entry to the app"
  [& _]
  (let [port (Integer/parseInt (or (System/getenv "PORT") "3000"))]
    ;; Run the server with the Ring.defaults middleware
    (reset! server (server/run-server
                    (middleware/wrap-defaults
                     #'api/app-routes
                     middleware/api-defaults)
                    {:port port}))
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
