(ns timesheet.legacy-parser
  (:require [clojure.string  :as str]
            [clojure.java.io :as io]
            [timesheet.time  :as time]))

;;; parsing
(defn new-group-line? [line]
  (-> line
      str/trim
      (str/starts-with? "{")))

(defn end-group-line? [line]
  (-> line
      str/trim
      (str/starts-with? "}")))

(defn comment-line? [line]
  (-> line
      str/trim
      (str/starts-with? "#")))

(defn new-task-line? [line]
  (let [timestamp "[0-9]{2}:[0-9]{2}[ ]+"
        time-sep "--[ ]+"
        desc "\\*[ ]+.*$"]
    (-> (str timestamp time-sep timestamp desc)
        re-pattern
        (re-matches line))))

(defn ^:private line-type [line]
  (cond (new-group-line? line) :new-group
        (new-task-line?  line) :new-task
        (end-group-line? line) :end-group
        (comment-line? line)   :empty-line
        :else :empty-line))

(defmulti parse-line
  {:arglists '([ctx line])}
  (fn [_ line]
    (line-type line)))


(defmethod parse-line :new-group
  [ctx line]
  (if (get ctx :within-group?)
    (throw
     (ex-info (format "Cannot parse \"%s\" while in group" line)
              {:caused-by line}))
    (let [group (str/trim (subs line 1))]
      (assoc ctx
             :current-group group
             :within-group? true ))))

(defmethod parse-line :new-task
  [ctx line]
  (if (get ctx :within-group?)
    (let [[time description] (str/split line #"\*" 2)
          [start end] (str/split time #"--")]

      (update ctx :current-tasks (fnil conj [])
              {:start (str/trim start)
               :end   (str/trim end)
               :description (str/trim description)}))
    (throw
     (ex-info (format "Cannot parse \"%s\" outside of group" line)
              {:caused-by line}))))

(defmethod parse-line :end-group
  [ctx _]
  (if (get ctx :within-group?)
    (let [{:keys [current-group current-tasks]} ctx
          task-group {:group current-group
                      :tasks current-tasks}]
      (-> ctx
          (update :task-groups (fnil conj []) task-group)
          (assoc  :within-group? false
                  :current-group nil
                  :current-tasks [])))
    ctx))

(defmethod parse-line :empty-line
  [ctx _]
  ctx)

(defn parse [string]
  (let [ctx {:within-group? false}
        lines (str/split-lines string)]
    (reduce (fn [result line] (parse-line result line)) ctx lines)))

(defn parse-file [filepath]
  (let [date (last (str/split filepath #"/"))]
    (-> filepath
        slurp
        parse
        (assoc :date date)
        (select-keys [:task-groups :date]))))

;;; Deal with dates

(defn parse-files
  [directory start end]
  (loop [date start
         result []]
    (if (time/after? date end)
      result
      (let [filepath (str directory "/" date)
            next-date (time/next-day date)]
        (if (.exists (io/file filepath))
          (recur next-date (->> filepath parse-file (conj result)))
          (recur next-date result))))))

;;; Data Cleaning :(

(defn company-at-date
  [date]
  (let [csi-start   "05-01-2019"
        reify-start "05-07-2021"
        cisco-start "02-13-2023"]
    (cond
      (time/before? date csi-start) :default
      (and (time/after? date csi-start)
           (time/before? date reify-start))
      :csi

      (and (time/after? date reify-start)
           (time/before? date cisco-start))
      :reify

      (time/after? date cisco-start) :cisco

      :else :default)))

(defmulti update-group-name
  {:arglists '(date group)}
  (fn [date _]
    (company-at-date date)))

(defmethod update-group-name :default
  [_ group]
  group)

(comment
  (def csi-dates (parse-files "/Users/andrewparisi/Documents/records/timesheet"
                              "04-20-2019" "05-06-2021"))

  (def reify-dates  (parse-files "/Users/andrewparisi/Documents/records/timesheet"
                                 "05-07-2021" "12-31-2022"))

  (map :date (filter (fn [date] (contains? (into #{} (map :group (get date :task-groups)))
                                          "Klezmer Archives"


                                          )) reify-dates))

  (->> reify-dates (mapcat :task-groups) (map :group) distinct)
  )
