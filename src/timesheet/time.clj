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

(defn date-earlier-than
  "Compare two dates. Return true if
   the first is earlier than the second.
   Otherwise return false."
  [date-one date-two]
  (let [{:keys [year-one month-one day-one]} date-one
        {:keys [year-two month-two day-two]} date-two]
    (cond
      (< year-one year-two) true
      (> year-one year-two) false
      :else (cond
              (< month-one month-two) true
              (> month-one month-two) false
              :else (if (< day-one day-two)
                      true
                      false)))))
