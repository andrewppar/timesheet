(ns timesheet.core
  (:gen-class)
  (:require [timesheet.db :as db]
            [timesheet.serialize_tasks :as serialize]))

(defn -main
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
