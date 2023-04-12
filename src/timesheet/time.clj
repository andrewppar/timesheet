(ns timesheet.time
  (:import (java.time LocalDate LocalTime)
           (java.time.format DateTimeFormatter)))

(def ^:prviate date-formatter
  (DateTimeFormatter/ofPattern "MM-dd-yyyy"))

;; This may have to be a multimethod one day
;; if we have different formatters
(defn ^:private to-date [string]
  (LocalDate/parse string date-formatter))

(defn temporal-type
  [object]
  (cond (try (to-date object) (catch Exception _e false)) :date
        (try (LocalTime/parse object) (catch Exception _e false)) :time
        :else
        (throw
         (ex-info
          (format "Cannot parse \"%s\" into either date or time." object)
          ;; TODO: Put soething meaningful here
          {}))))

(defmulti before?
  {:arglists '([item-one time-two])}
  (fn [item-one item-two]
    (if (= (temporal-type item-one) (temporal-type item-two))
      (temporal-type item-one)
      (throw
       (ex-info "Cannot compare temporal objects of different types"
                {:caused-by `(= (type ~item-one) (type ~item-two))
                 :type-one   (type item-one)
                 :type-two   (type item-two)})))))

;; Time

(defmethod before? :time
  [time-one time-two]
  (.isBefore (LocalTime/parse time-one)
             (LocalTime/parse time-two)))

;; Date

(defn ^:private show-date [date]
  (.format date-formatter date))

(defn plus-days
  "Get `n` days after `date`.

   Note: `date` is expected to be in \"MM-dd-yyyy\" format."
  [date n]
  (-> date
      to-date
      (.plusDays n)
      show-date))

(defn next-day
  "Get the day after `date`.

   Note: `date` is expected to be in \"MM-dd-yyyy\" format."
  [date]
  (plus-days date 1))

(defn today
  "Get the \"MM-dd-yyyy\" representation of today"
  []
  (.format date-formatter (LocalDate/now)))

(defmethod before? :date
  [date-one date-two]
  (.isBefore (to-date date-one)
             (to-date date-two)))

;; Generic

(defn after?
  "Check whether `object-one` and `object-two` are of the same type
   and whether the first comes after the second."
  [object-one object-two]
  (not
   (or
    (= object-one object-two)
    (before? object-one object-two))))
