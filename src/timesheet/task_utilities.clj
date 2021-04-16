(ns timesheet.task_utilities
  (:require [timesheet.task :as task]))

(defn sum-tasks
  "Generate the total time for
   a list of tasks in hours"
  [tasks]
  (/ (reduce
      (fn [acc task]
        (+ acc (:total-time task)))
      0 tasks)
     60.0))
