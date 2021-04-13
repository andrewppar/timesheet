(ns timesheet.time-test
  (:require [clojure.test :refer :all]
            [timesheet.time :refer :all]))


(deftest new-time-from-string-test
  (let [result (new-time-from-string "10:11")]
    (is (= (->Time 10 11) result))))

(deftest new-time-from-string-bad-minute
  (is (thrown? Exception  (new-time-from-string "10:79"))))

(deftest new-time-from-string-bad-hour
  (is (thrown? Exception (new-time-from-string "41:12"))))

(deftest new-date-from-string-test
  (let [result (new-date-from-string "10-11-1987")]
    (is (= (->Date 1987 10 11) result))))

(deftest new-date-from-string-bad-month
  (is (thrown? Exception (new-date-from-string "13-12-1066"))))

(deftest new-date-from-string-bad-day
  (is (thrown? Exception (new-date-from-string "10-34-1203"))))

(deftest new-date-from-string-low-month
  (is (thrown? Exception (new-date-from-string "0-12-2020"))))

(deftest new-date-from-string-december
  (new-date-from-string "12-25-2021"))

(deftest new-date-from-string-thirty-one
  (new-date-from-string "12-31-2021"))

(deftest date-earlier-than-test-one
  (let [date-one (->Date 1987 10 11)
        date-two (->Date 1989 12 5)]
    (is (date-earlier-than date-one date-two))
    (is (not (date-earlier-than date-two date-one)))
    (is (not (date-earlier-than date-one date-one)))
    (is (not (date-earlier-than date-two date-two)))))

(deftest date-earlier-than-test-two
  (let [date-one (->Date 1987 10 11)
        date-two (->Date 1987 12 5)]
    (is (date-earlier-than date-one date-two))
    (is (not (date-earlier-than date-two date-one)))
    (is (not (date-earlier-than date-one date-one)))
    (is (not (date-earlier-than date-two date-two)))))

(deftest date-earlier-than-test-three
  (let [date-one (->Date 1987 12 10)
        date-two (->Date 1987 12 11)]
    (is (date-earlier-than date-one date-two))
    (is (not (date-earlier-than date-two date-one)))
    (is (not (date-earlier-than date-one date-one)))
    (is (not (date-earlier-than date-two date-two)))))

(deftest date-earlier-than-test-four
  (let [date-one (->Date 1987 12 11)
        date-two (->Date 1987 12 11)]
    (is (not (date-earlier-than date-one date-two)))
    (is (not (date-earlier-than date-two date-one)))
    (is (not (date-earlier-than date-one date-one)))
    (is (not (date-earlier-than date-two date-two)))))
