(ns timesheet.migrate
  (:require
   [clojure.java.io :as io]
   [timesheet.assert :as assert]
   [timesheet.legacy-parser :as lp]
   [timesheet.time :as time]))


;; TODO: These might be specific to v1 on disk
;; That said, as far as I know, I'm the only one
;; who uses this - so that's all the migration
;; necessary
(defn ^:private task-to-assert-spec
  [date group {:keys [start end description]}]
  [date start end group description])

(defn ^:private task-group-to-assert-specs
  [date {:keys [group tasks]}]
  (map (partial task-to-assert-spec date group) tasks))

(defn task-dates-to-assert-specs
  [{:keys [date task-groups]}]
  (mapcat (partial task-group-to-assert-specs date) task-groups))

(defn migrate-legacy-db
  [directory start end]
  (let [batch 100]
    (loop [current-date start]
      (when-not (time/after? current-date end)
        (let [next-date  (time/plus-days current-date batch)
              task-dates (lp/parse-files
                          directory current-date next-date)
              asserts    (mapcat task-dates-to-assert-specs task-dates)]
          (apply assert/assert! asserts)
          (recur next-date))))))

(defn ^:private location-type
  [location]
  (cond
    (and (string? location)
         (.exists (io/file location)))
    :on-disk))

(defmulti run!
  {:arglists '([db-type location])}
  (fn [db-type location]
    [db-type (location-type location)]))

(defmethod run! [:v1 :on-disk]
  [_ location]
  (migrate-legacy-db
   location "10-11-1987" (time/today)))

(defmethod run! [:v2 :on-disk]
  [_ location]
  ;; TODO: Support this at some point
  ())

(defmethod run! [:v2 :network]
  [_ location]
  ())







(comment
  (migrate-legacy-db
   "/Users/andrewparisi/Documents/records/timesheet"
   "03-01-2019" "07-01-2023")
  )
