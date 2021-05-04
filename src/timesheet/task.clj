(ns timesheet.task
  (:require [timesheet.time :as time]))

(defrecord Task [start end date task-group description total-time])

(defn new-task
  "Constructor for the Task record"
  [start-string end-string date-string group description]
  (let [start (time/new-time-from-string start-string)
        end   (time/new-time-from-string end-string)
        date  (time/new-date-from-string date-string)
        start-minutes (+ (:minute start)
                         (* 60 (:hour start)))
        end-minutes   (+ (:minute end)
                         (* 60 (:hour end)))]
    (when (< end-minutes start-minutes)
      (throw
       (ex-info "Task ends before it starts" {})))
    (let [total-minutes (- end-minutes start-minutes)]
      (->Task start end date group description total-minutes))))


(defn sum-tasks
  "Generate the total time for
   a list of tasks in hours"
  [tasks]
  (/ (reduce
      (fn [acc task]
        (+ acc (:total-time task)))
      0 tasks)
     60.0))

(defn task-earlier?
  "Check whether the first task
  started earlier than the second"
  [task-one task-two]
  (let [start-one (:start task-one)
        start-two (:start task-two)]
    (time/earlier? start-one start-two)))
