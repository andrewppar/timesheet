(ns timesheet.server
  (:require
   [org.httpkit.server       :as server]
   [ring.middleware.defaults :as middleware]
   [ring.middleware.json     :as json]
   [timesheet.log            :refer [defn-logged]]
   ))

(defonce server (atom nil))

(defn-logged start!
  "Start the timesheet server."
  {:level :info}
  [routes port]
  (reset! server
          (-> routes
              (middleware/wrap-defaults middleware/api-defaults)
              (server/run-server {:port port}))))


(defn stop! []
  (when-not (nil? @server)
    (@server :timeout 100)
    #_(logging)
    (reset! server nil)))
