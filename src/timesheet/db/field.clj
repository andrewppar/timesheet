(ns timesheet.db.field
  (:require [clojure.math :as math]
            [timesheet.utils :as utils])
  (:import (org.apache.lucene.document
            Field$Store IntPoint StoredField StringField TextField)))


(defmulti validate
  "Validate an input to be indexed"
  {:arglists '([value value-type])}
  (fn [_ value-type]
    value-type))

(defmethod validate :string
  [value _]
  (assert (string? value)
          (format "Cannot parse. %s is not a string" value))
  value)

(defmethod validate :text
  [value _]
  (validate value :string))


;;; TODO: Should some of this be moved to time.clj?
(defmethod validate :date
  [value _]
  (assert (int? value))
  (let [date-string (str value)
        month       (Integer/parseInt (subs date-string 4 6))
        day         (Integer/parseInt (subs date-string 6 8))]
    (assert (and (<= 0 month 12) (<= 0 day 31))
            (format "Cannot create date for %s" date-string))
    value))

(defmethod validate :time
  [value _]
  (assert (and (>= value 0) (< value 2400) (number? value))
          (format "Cannot make field. %s is not a valid time" value))
  (let [num-zeros   (if (= value 0) 4 (- 3 (math/floor (math/log10 value))))
        time-string (str (utils/make-string num-zeros \0) value)
        minutes     (-> time-string
                        (subs 2 4)
                        Integer/parseInt)]
    (assert (<= 0 minutes 59)
            (format "Cannot create valid timestamp for %s" time-string))
    value))

(defmulti ^:private field
  "Create a `field` for the `field-type` with `value`"
  {:arglists '([field-name value field-type])}
  (fn [_ _ field-type]
    field-type))

(defmethod field :string
  [field-name value _]
  [(StringField. field-name value Field$Store/YES)])

(defmethod field :text
  [field-name value _]
  [(TextField. field-name value Field$Store/YES)])

(defmethod field :date
  [field-name value _]
  (let [int-field    (IntPoint. field-name (int-array [value]))
        stored-field (StoredField. field-name value)]
    [int-field
     stored-field]))

(defmethod field :time
  [field-name value _]
  (field field-name value :date))

(defn make
  "Create a new field for a document.

  Each field has a `field-name`, a `value`, and a `field-type`.
  `field-name` is a string, `value` is an instance of the
  appropriate type, and `field-type` is the type that
  value should be stored as in lucene."
  [field-name value field-type]
  (validate value field-type)
  (field field-name value field-type))
