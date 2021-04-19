(ns timesheet.db_test
  (:require [clojure.test :refer :all]
            [timesheet.task :refer :all]
            [timesheet.db :refer :all]))

(deftest parse-line-not-within-group
  (let [old-parser (->Parser [] "12-05-1989" "group" false)
        line       "a random line"]
    (is (= old-parser (parse-line old-parser line)))))

(deftest parse-line-to-group
  (let [old-parser  (->Parser [] "12-05-1989" "Group" false)
        line       "     {New Group "
        new-parser (->Parser [] "12-05-1989" "New Group" true)]
    (is (= new-parser (parse-line old-parser line)))))

(deftest parse-line-new-item
  (let [old-parser (->Parser [] "12-05-1989" "Group" true)
        line       "10:00 -- 12:00 * Description"
        new-task   (new-task
                    "10:00"
                    "12:00"
                    "12-05-1989"
                    "Group"
                    "Description")
        new-parser (->Parser [new-task] "12-05-1989" "Group" true)]
    (is (= new-parser (parse-line old-parser line)))))

(deftest parse-line-bad-time-one
  (let [old-parser (->Parser [] "12-05-1989" "Group" true)
        line-one   "25:00 -- 26:00 * Test"
        line-two   "10:120 -- 12:00 * Test"
        line-three "10:00 -- 09:00 * Test"]
    (is (thrown? Exception (parse-line old-parser line-one)))
    (is (thrown? Exception (parse-line old-parser line-two)))
    (is (thrown? Exception (parse-line old-parser line-three)))))

(deftest parse-line-exception-one
  (let [old-parser (->Parser [] "12-05-1989" "Group" true)
        line       "{new group"]
    (is (thrown? Exception (parse-line old-parser line)))))
