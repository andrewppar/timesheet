(ns timesheet.integration-test
  (:require
   [clojure.test          :refer [deftest is use-fixtures]]
   [timesheet.query       :as query]
   [timesheet.core        :as core]
   [timesheet.test-config :as cfg]
   [timesheet.assert      :as assert]))

(use-fixtures :once cfg/wrap-setup)

(deftest assert-test
  (let [date "03-22-2023"]
    (assert/assert! [date "21:00" "21:30" "Test" "Ran a test"])
    (let [result (query/run ["Test"] date date)]
      (is (> (count result) 0)))))

(deftest retract-test
  (let [date "03-22-2032"]
    (assert/retract! [date "21:00" "21:30"])
    (let [result (query/run ["Test"] date date)]
      (is (= (count result) 0)))))
