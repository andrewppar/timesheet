(ns timesheet.db.document
  (:require [timesheet.db.field :as field])
  (:import
   (org.apache.lucene.document Document)))

(defn add-field!
  [doc field value field-type]
  (reduce
   (fn [_ to-add]
     (.add doc to-add))
   doc
   (field/make field value field-type))
 doc)

(defn taxonomy [subgroup supergroup]
  (let [doc (Document.)]
    (add-field! doc "type" "taxonomy" :string)
    (add-field! doc "subgroup" subgroup :string)
    (add-field! doc "supergroup" supergroup :string)))

(defn task
  [date start end group description]
  (let [doc (Document.)]
    (add-field! doc "type"        "task"      :string)
    (add-field! doc "date"        date        :date)
    (add-field! doc "start"       start       :time)
    (add-field! doc "end"         end         :time)
    (add-field! doc "group"       group       :string)
    (add-field! doc "description" description :text)))
