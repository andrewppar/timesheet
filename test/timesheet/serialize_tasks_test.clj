(ns timesheet.serialize-tasks-test
  (:require [clojure.test :refer :all]
            [timesheet.task :refer :all]
            [timesheet.serialize_tasks :refer :all]))

(deftest serialize-task-to-file-format-test
  (is (= (serialize-task-to-file-format
          (new-task "10:30" "11:30" "12-12-2021" "Group" "A task"))
         "10:30 -- 11:30 * A task\n")))

(deftest serialize-tasks-file-format-test-one
  (let [tasks [(new-task "11:30" "12:30" "12-13-2021" "Group" "Another Task")
               (new-task "10:30" "11:30" "12-12-2021" "Group" "A task")]]
    (is (thrown? Exception (to-file-format tasks)))))

(deftest serialize-tasks-file-format-test-one
  (let [tasks [(new-task "11:30" "12:30" "12-13-2021" "Group" "Another Task")
               (new-task "10:30" "11:30" "12-13-2021" "Group" "A task")]]
    (is (= (to-file-format tasks)
           "{Group\n11:30 -- 12:30 * Another Task\n10:30 -- 11:30 * A task\n}\n\n"))))
