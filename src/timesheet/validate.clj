(ns timesheet.validate
  (:require
   [timesheet.task :as task]))

(defn check-for-gaps
  "Given a list of tasks
  return lists of start
  and end times where no activity was done
  "
  [tasks]
  (let [sorted-tasks (sort  task/task-earlier? tasks)
        [gaps _] (reduce (fn [[gaps last-task] task]
                           (let [last-end  (:end last-task)
                                 new-start (:start task)
                                 new-gaps (if (and last-task
                                                   (not (= last-end new-start)))
                                            (conj gaps {:start last-end :end new-start})
                                            gaps)]
                             [new-gaps task]))
                         [[] nil] sorted-tasks)]
    gaps))
