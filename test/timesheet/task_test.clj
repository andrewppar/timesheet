(ns timesheet.task_test
  (:require [clojure.test :refer :all]
            [timesheet.task :refer :all]
            [timesheet.time :refer :all]))

(deftest new-task-test
  (let [result (new-task "10:30" "12:30" "01-07-2020" "Cool" "new house")
        expect (->Task
                (->Time 10 30)
                (->Time 12 30)
                (->Date 2020 1 7)
                "Cool"
                "new house"
                120)]
    (is (= result expect))))

(deftest new-task-bad-times
  (is
   (thrown? Exception
    (new-task "10:30" "09:00" "01-07-2020" "Cool" "new house"))))
