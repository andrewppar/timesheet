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
    (if (< end-minutes start-minutes)
      (throw
       (ex-info "Task ends before it starts" {})))
    (let [total-minutes (- end-minutes start-minutes)]
      (->Task start end date group description total-minutes))))
