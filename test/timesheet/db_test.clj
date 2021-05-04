(ns timesheet.db_test
  (:require
            [clojure.test :refer [deftest is use-fixtures]]
            [timesheet.db :refer [->Parser
                                  parse-line
                                  parse-task-file
                                  delete-task
                                  add-task]]
            [timesheet.globals :refer [db-root]]
            [timesheet.task :refer [new-task]]))

(def test-db-root  "test/timesheet/test_assets/")

(defn set-up [test-function]
  (with-redefs [db-root test-db-root]
    (spit (str db-root "01-07-2020")
          (str "{Intro\n"
               "09:00 -- 09:30 * A Clean Test\n"
               "10:00 -- 11:00 * Another task \n"
               "}\n")))
  (test-function)
  (with-redefs [db-root test-db-root]
    (spit (str db-root "01-07-2020")
          (str "{Intro\n"
               "09:00 -- 09:30 * A Clean Test\n"
               "}\n"))))

(use-fixtures :each set-up)

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

(deftest add-task-one
  (with-redefs [db-root test-db-root]
    (let [date          "01-07-2020"
          initial-count (->> (str db-root date)
                             parse-task-file
                             count)]
      (add-task  date "08:00" "09:00" "Life" "Buy a House")
      (is (= (+ initial-count 1) (->> (str db-root date)
                                      parse-task-file
                                      count))))))

(deftest delete-task-one
  (with-redefs [db-root test-db-root]
    (let [date "01-07-2020"
          initial-count (->> (str db-root date)
                             parse-task-file
                             count)]
      (delete-task date "09:00" "09:30")
      (is (= (- initial-count 1) (->> (str db-root date)
                                      parse-task-file
                                      count))))))
