(ns timesheet.validate_test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [timesheet.db   :refer [parse-task-file]]
            [timesheet.time :refer [new-time-from-string]]
            [timesheet.validate :refer [check-for-gaps]]
            [timesheet.globals :refer [db-root]]))


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
               "10:00 -- 11:00 * Another task \n"
               "}\n"))))

(use-fixtures :each set-up)

(deftest find-gaps-one
  (with-redefs [db-root test-db-root]
    (let [gaps (->> (str db-root "01-07-2020")
                    parse-task-file
                    check-for-gaps)]
      (is (= [{:start (new-time-from-string "09:30")
               :end (new-time-from-string "10:00")}] gaps )))))

