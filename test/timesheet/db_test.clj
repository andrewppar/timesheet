(ns timesheet.db_test
  (:require [clojure.test :refer :all]
            [timesheet.db :refer :all]))


(deftest week-internal-one
  (is (= '("02-27-2021"
           "02-28-2021"
           "03-01-2021"
           "03-02-2021"
           "03-03-2021"
           "03-04-2021"
           "03-05-2021")
         (week-internal 2 27))))

(deftest week-internal-two
  (is (= '("03-31-2021"
           "04-01-2021"
           "04-02-2021"
           "04-03-2021"
           "04-04-2021"
           "04-05-2021"
           "04-06-2021")
         (week-internal 3 31))))

(deftest week-internal-three
  (is (= '("04-01-2021"
           "04-02-2021"
           "04-03-2021"
           "04-04-2021"
           "04-05-2021"
           "04-06-2021"
           "04-07-2021")
         (week-internal 4 1))))

(deftest week-internal-four
  (is (= '("04-05-2021"
           "04-06-2021"
           "04-07-2021"
           "04-08-2021"
           "04-09-2021"
           "04-10-2021"
           "04-11-2021")
         (week-internal 4 5))))

(deftest week-internal-five
  (is (= '("09-27-2021"
           "09-28-2021"
           "09-29-2021"
           "09-30-2021"
           "10-01-2021"
           "10-02-2021"
           "10-03-2021")
         (week-internal 9 27))))

(deftest week-internal-six
  (is (= '("11-01-2021"
           "11-02-2021"
           "11-03-2021"
           "11-04-2021"
           "11-05-2021"
           "11-06-2021"
           "11-07-2021")
         (week-internal 11 1))))
