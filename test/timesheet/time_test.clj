(ns timesheet.time-test
  (:require [clojure.test :refer [deftest are testing]]
            [timesheet.time :as time]))

(deftest test-before?
  (testing "positive cases"
    (are [start end]
        (time/before? start end)
        "10:00" "10:01"
        "10:00" "11:00"))
  (testing "negative cases"
    (are [start end]
        (not (time/before? start end))
        "10:01" "10:00"
        "11:00" "10:00"
        "10:00" "10:00")))
