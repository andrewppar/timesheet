(ns timesheet.time
  (:require [clojure.string :refer [split]]))

(defrecord Time [hour minute]
  Object
  (toString [_]
    (str hour ":" minute)))

(defn new-time-from-string
  "Contructor for the Time record from string"
  [string]
  (let [hour-minute (split string #":")
        hour   (->> hour-minute first Integer/parseInt)
        minute (->> hour-minute second Integer/parseInt)]
    (when (or
           (< hour 0)
           (> hour 23))
      (throw
       (ex-info "Cannot parse hour-string to time"
                {:causes #{:hour hour}})))
    (when (or
           (< minute 0)
           (> minute 60))
      (throw
       (ex-info "Cannot parse minute-string to time"
                {:causes #{:minute minute}})))
    (->Time hour minute)))

;;;;;;;;;;;;;
;;; Dates ;;;
;;;;;;;;;;;;;

(defrecord Date [year month day]
  Object
  (toString [_]
    (str month "-" day "-" year)))

(defn new-date-from-string
  "Constructor for the Date record from string

  Expects dates of the form MM-DD-YYYY"
  [string]
  (let [month-day-year (split string #"-")
        month (->> month-day-year first Integer/parseInt)
        day   (->> month-day-year second Integer/parseInt)
        year  (Integer/parseInt (nth month-day-year 2))]
    (when (or (< month 1)
              (> month 12))
      (throw
       (ex-info "Cannot parse month"
                {:causes #{:month month}})))
    (when (or (< day 0)
              (> day 31))
      (throw
       (ex-info "Canonot parse day"
                {:causes #{:day day}})))
    ;; @todo write up a check for appropriate day for month,
    ;; e.g. rule out 2-31

    (->Date year month day)))

(def month-threshold-map
  "A map of months to the number of days in them"
  {(keyword (str 1)), 31
   (keyword (str 2)), 28
   (keyword (str 3)), 31
   (keyword (str 4)), 30
   (keyword (str 5)), 31
   (keyword (str 6)), 30
   (keyword (str 7)), 31
   (keyword (str 8)), 31
   (keyword (str 9)), 30
   (keyword (str 10)), 31
   (keyword (str 11)), 30
   (keyword (str 12)), 31})


(defn leap-year?
  "Returns whether or not
  an int representing a year
  is a leap-year"
  [year]
  (=  (mod year 4) 0))


(defn valid-date?
  "Returns whether or not a
  date record actually represents a date"
  [date]
  (let [{:keys [year month day]} date]
    (cond  (and
            (leap-year? year)
            (= month 2)
            (= day 29)) true
           (or
            (< month 1)
            (> month 12)
            (< day 1)) false
           :else (<= day
                     ((keyword (str month))
                      month-threshold-map)))))

(defn next-date
  "Given a date get the next calendar date"
  [date]
  (if (not (valid-date? date))
    (throw
     (Exception. (format "%s is not a valid date" date)))
    (let [{:keys [year month day]} date
          max-day ((keyword (str month)) month-threshold-map)]
      (if (not (= day max-day))
        (->Date year month (+ day 1))
        (cond (= month 12) (->Date (+ year 1) 1 1)
              (and (= month 2) (leap-year? year)) (->Date year 2 29)
              :else (->Date year (+ month 1) (+ day 1)))))))

;; Date Comparator
(defn date-earlier-than
  "Compare two dates. Return true if
   the first is earlier than the second.
   Otherwise return false."
  [date-one date-two]
  (let [year-one  (:year date-one)
        year-two  (:year date-two)
        month-one (:month date-one)
        month-two (:month date-two)
        day-one   (:day date-one)
        day-two   (:day date-two)]
    (cond
      (< year-one year-two) true
      (> year-one year-two) false
      :else (cond
              (< month-one month-two) true
              (> month-one month-two) false
              :else (if (< day-one day-two)
                      true
                      false)))))

;; Date Utilities
(defn today
  "Get todays date"
  []
  (let [date-string (.format
                     (java.text.SimpleDateFormat.
                      "MM-dd-yyyy")
                     (new java.util.Date))]
    (new-date-from-string date-string)))





(defn -week
  "Given a month and day generate the next
   seven days"
  [month day]
  (map
   (fn [offset]
     (let [month-threshold ((keyword (str month)) month-threshold-map)
           new-month    (if (> (+ offset day) month-threshold)
                          (+ month 1)
                          month)
           new-day      (if (> (+ day offset) month-threshold)
                          (- (+ day offset) month-threshold)
                          (+ day offset))
           month-string (if (> 10 new-month) (str "0" new-month) (str new-month))
           day-string   (if (> 10 new-day) (str "0" new-day) (str new-day))]
       (format "%s-%s-2021" month-string day-string)))
   [0 1 2 3 4 5 6]))

(defn dates-in-range
  "Get all the dates starting
  from start-date until end-date
  inclusive"
  [start-date end-date]
  (loop [current-date start-date acc []]
    (if (= current-date end-date)
      (conj current-date acc)
      (recur (next-date current-date) (conj current-date acc)))))
