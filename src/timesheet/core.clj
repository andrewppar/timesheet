(ns timesheet.core
  (:gen-class)
  (:require [clojure.string :refer [split
                                    trim
                                    starts-with?
                                    join
                                    split-lines
                                    ]]
            [clojure.java.io :as io]
            [timesheet.task :as task]))


(defrecord Parser [tasks current-date current-group within-group?])

(defn new-task-from-line
  "Create a task from a group string and a line"
  [line group date]
  (let [divided-string (split line #"\*")
        description (->> divided-string second trim)
        time-group  (->> divided-string first trim)
        start-end   (map trim (split time-group #"--"))
        start-time  (first start-end)
        end-time    (second start-end)]
    (task/new-task start-time end-time date group description)))

(defn parse-line
  "Upadte parser with a single line"
  [parser line]
  (let [clean (trim line)]
    (cond (starts-with? clean "{")
          (if (:within-group? parser)
            (throw
             (Exception. "Could not parse line: nested {"))
            (let [group (join
                         " " (rest
                              (split line #"\{")))
                  clean-group (trim group)]
              (assoc parser
                     :current-group clean-group
                     :within-group? true)))
          (starts-with? clean "}")
          (assoc parser :within-group? false)
          (:within-group? parser)
          (let [group (:current-group parser)
                date  (:current-date  parser)
                new-task (new-task-from-line line group date)
                new-tasks (conj (:tasks parser) new-task)]
            (assoc parser :tasks new-tasks))
          :else parser
          )))

(defn parse-task-file
  "Parse a list of tasks from a file

  Takes a file and returns a vector of
  Task records

  Expects the file itself to be a date
  of the form MM-DD-YYYY"
  [filepath]
  (if (->> filepath
           io/as-file
           .exists
           not)
    '()
    (let [file-string (slurp filepath)
          as-lines    (split-lines file-string)
          date        (last (split filepath #"\/"))
          parser      (->Parser [] date "" false)]
      (:tasks (reduce parse-line parser as-lines)))))

(defn sum-tasks
  "Generate the total time for
   a list of tasks in hours"
  [tasks]
  (/ (reduce
      (fn [acc task]
        (+ acc (:total-time task)))
      0 tasks)
     60.0))

(defn -generate-group
  "Generates the string representation
  of a group."
  [group group-column-length]
  (let [group-length (count group)]
    (if (<= group-length group-column-length)
      (let [padding-count (-
                           (+ 5 group-column-length)
                           group-length)]
        (str group (apply str (repeat padding-count " "))))
      (str (subs group 0 group-column-length) "...  "))))

(defn -generate-first-description-line
  "Generates the first line
  of a description"
  [descriptions description-column-length]
  (let [description (first descriptions)
        desc-size   (count description)]
    (if (<= desc-size description-column-length)
      (let [padding-count (- (+ 5 description-column-length)
                             desc-size)]
        (str description
             (apply
              str
              (repeat padding-count " "))))
      (str (subs description 0 description-column-length) "...  "))))

(defn -generate-postfixes
  "Generates the strings to which
   dates are appended.
  "
  [groups group-column-length description-column-length]
  (map
   (fn [group]
     (let [task-group (first group)
           group-tasks (second group)
           truncated-group (-generate-group task-group group-column-length)
           group-descriptions (map :description group-tasks)
           truncated-description (-generate-first-description-line
                                  group-descriptions description-column-length)
           total-time         (sum-tasks group-tasks)]
       (str " " truncated-group " " truncated-description " " total-time "\n")))
   groups))

(defn show-tasks-at-date
  "Shows tasks associated with
   a date.

   Note: assumes that all the tasks
   that it is passed are in fact
   associated with the date it is
   passed"
  ;; Options to handle next:
  ;; 1. Don't truncate descriptions
  ;; 2. Don't truncate task group
  [date tasks]
  (let [groups (group-by :task-group tasks)
        group-length 20
        description-length 40
        postfixes  (-generate-postfixes groups group-length description-length)
        date-padding   (apply str (repeat (+ (count date) 7) " "))
        get-padding    (fn [date type] (when (< (type date) 10) " "))
        start (str
               date
               (get-padding date :day)
               (get-padding date :month)
               (first postfixes))
        rest  (reduce (fn [acc task-string] (str acc date-padding task-string)) "" (rest postfixes))]
    (str start rest)))



;;; Showing Tasks
;;@todo Consider whether there's a better place for this
(defn show-tasks
  "Generate a string representation of
  a list of tasks

  Takes a list of tasks and generates a
  string representation of them."
  [tasks]
  (let [dates (group-by :date tasks)]
    (str
     (reduce
     (fn [acc group]
       (let [date (first group)
             date-tasks (second group)]
         (str acc
              (show-tasks-at-date date date-tasks))))
     "" dates)
     "\n"
     (apply str (repeat 90 "-"))
     "\n"
     (sum-tasks tasks))))

(defn -main
  [date]
  (let [root  "/Users/andrew/Documents/Records_flat"
        filepath (str root "/" date)]
    (->> filepath
         parse-task-file
         show-tasks
         println)))
