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

(deftest next-date-test
  (do
    (is (= (next-date (->Date 2020 12 31)) (->Date 2021 1 1)))
    (is (= (next-date (->Date 1987 10 10)) (->Date 1987 10 11)))
    (is (= (next-date (->Date 1988 1  31)) (->Date 1988 2 1)))
    (is (= (next-date (->Date 1989 2  28)) (->Date 1989 3 1)))
    (is (= (next-date (->Date 1990 3  31)) (->Date 1990 4 1)))
    (is (= (next-date (->Date 1991 4  30)) (->Date 1991 5 1)))
    (is (= (next-date (->Date 1992 5  31)) (->Date 1992 6 1)))
    (is (= (next-date (->Date 1993 6  30)) (->Date 1993 7 1)))
    (is (= (next-date (->Date 1994 7  31)) (->Date 1994 8 1)))
    (is (= (next-date (->Date 1995 8  31)) (->Date 1995 9 1)))
    (is (= (next-date (->Date 1996 9  30)) (->Date 1996 10 1)))
    (is (= (next-date (->Date 1997 10 31)) (->Date 1997 11 1)))
    (is (= (next-date (->Date 1998 11 30)) (->Date 1998 12 1)))
    (is (= (next-date (->Date 1999 12 31)) (->Date 2000 1 1)))
    (is (thrown? Exception
                 (next-date (->Date 10 35 2021))))
    (is (thrown? Exception
                 (next-date (->Date 2021 2 29 ))))
    (is (= (next-date (->Date 2020 2 29)) (->Date 2020 3 1)))
    (is (= (next-date (->Date 2000 2 28)) (->Date 2000 2 29)))))



(deftest week-internal-one
  (is (= '("02-27-2021"
           "02-28-2021"
           "03-01-2021"
           "03-02-2021"
           "03-03-2021"
           "03-04-2021"
           "03-05-2021")
         (-week 2 27))))

(deftest week-internal-two
  (is (= '("03-31-2021"
           "04-01-2021"
           "04-02-2021"
           "04-03-2021"
           "04-04-2021"
           "04-05-2021"
           "04-06-2021")
         (-week 3 31))))

(deftest week-internal-three
  (is (= '("04-01-2021"
           "04-02-2021"
           "04-03-2021"
           "04-04-2021"
           "04-05-2021"
           "04-06-2021"
           "04-07-2021")
         (-week 4 1))))

(deftest week-internal-four
  (is (= '("04-05-2021"
           "04-06-2021"
           "04-07-2021"
           "04-08-2021"
           "04-09-2021"
           "04-10-2021"
           "04-11-2021")
         (-week 4 5))))

(deftest week-internal-five
  (is (= '("09-27-2021"
           "09-28-2021"
           "09-29-2021"
           "09-30-2021"
           "10-01-2021"
           "10-02-2021"
           "10-03-2021")
         (-week 9 27))))

(deftest week-internal-six
  (is (= '("11-01-2021"
           "11-02-2021"
           "11-03-2021"
           "11-04-2021"
           "11-05-2021"
           "11-06-2021"
           "11-07-2021")
         (-week 11 1))))
