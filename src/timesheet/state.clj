(ns timesheet.state
  (:require
   [integrant.core     :as ig]
   [timesheet.config   :as cfg]
   [timesheet.db.index :as idx]
   [timesheet.taxonomy :as tax])
  (:import
   (java.io File)))

(defmethod ig/init-key ::taxonomy [_ _]
  (tax/group-taxonomy))

(def config
  {::taxonomy []})

(defn setup?
  "Check if timesheet has been setup on this computer"
  []
  (let [idx-loc (cfg/get :index-location)
        dir     (File. idx-loc)]
    (> 0 (.length dir))))

(def state
  (atom nil))

(defn init!
  "Start up the state for the taxonomy."
  []
  (reset! state (ig/init config)))

(defn stop!
  "Stop the state."
  []
  (reset! state nil))

(defn taxonomy
  "Return the taxonomy associated with state"
  []
  (get @state ::taxonomy))
