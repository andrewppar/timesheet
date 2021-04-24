(ns timesheet.serialize_tasks
  (:require [clojure.data.json :as json]
            [timesheet.task :as task]))


;;;;;;;;;;;;;;;
;;; Tabular ;;;
;;;;;;;;;;;;;;;

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
  [descriptions description-column-length truncate-descriptions?]
  (let [description (first descriptions)
        desc-size   (count description)]
    (if (<= desc-size description-column-length)
      (let [padding-count (- (+ 5 description-column-length)
                             desc-size)]
        (str description
             (apply
              str
              (repeat padding-count " "))))
      (str (subs description 0 description-column-length)
           (if truncate-descriptions?
             "...  "
             "")))))

(defn -generate-other-description-lines
  [group-descriptions description-column-length group-column-length]
  (let [date-padding (apply str (repeat 17 " "))
        group-padding (apply str (repeat (+ group-column-length 0) " "))
        all-descriptions (reduce str " " group-descriptions)
        split-descriptions (map (partial apply str)
                                (partition-all description-column-length all-descriptions))]
    (map
     (fn [description]
       (str date-padding group-padding description "\n"))
     (rest split-descriptions))))

(defn -generate-postfixes
  "Generates the strings to which
   dates are appended.
  "
  [groups group-column-length description-column-length truncate-descriptions?]
  (map
   (fn [group]
     (let [task-group (first group)
           group-tasks (second group)
           truncated-group (-generate-group task-group group-column-length)
           group-descriptions (map :description group-tasks)
           truncated-description (-generate-first-description-line
                                  group-descriptions description-column-length truncate-descriptions?)
           total-time         (task/sum-tasks group-tasks)
           first-line (str
                       " " truncated-group " " truncated-description " " total-time "\n")]
       (if truncate-descriptions?
         first-line
         (let [continued-descriptions (-generate-other-description-lines
                                       group-descriptions
                                       description-column-length
                                       group-column-length)]
           (reduce str first-line continued-descriptions)))))
   groups))

(defn tabular-show-tasks-at-date
  "Shows tasks associated with
   a date.

   Note: assumes that all the tasks
   that it is passed are in fact
   associated with the date it is
   passed"
  ;; Options to handle next:
  ;; 1. Don't truncate descriptions
  ;; 2. Don't truncate task group
  [date tasks truncate-descriptions?]
  (let [groups (group-by :task-group tasks)
        group-length 20
        description-length 40
        postfixes  (-generate-postfixes
                    groups group-length description-length truncate-descriptions?)
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
(defn tabular-show-tasks
  "Generate a string representation of
  a list of tasks

  Takes a list of tasks and generates a
  string representation of them."
  [tasks truncate-descriptions?]
  (let [dates (group-by :date tasks)]
    (str
     (reduce
      (fn [acc group]
        (let [date (first group)
              date-tasks (second group)]
          (str acc
               (tabular-show-tasks-at-date date date-tasks truncate-descriptions?))))
      "" dates)
     "\n"
     (apply str (repeat 90 "-"))
     "\n"
     (task/sum-tasks tasks))))

;;;;;;;;;;;
;;; org ;;;
;;;;;;;;;;;

(defn org-show-tasks-by-group
  "generate an org representation
  of tasks by group"
  [tasks]
  (let [groups (group-by :task-group tasks)]
    (map
     (fn [group]
       (let [descriptions (map :description (second group))]
         (str "** "
              (first group)
              "\n"
              (reduce str ""
                      (map (fn [description]
                             (str "*** " description "\n"))
                           descriptions)))))
     groups)))

(defn org-show-tasks-by-date
  "generate a string representation
  of a list of tasks as an org hierarchy. "
  [tasks]
  (let [dates (group-by :date tasks)]
    (reduce
     (fn [acc date]
       (let [groups (org-show-tasks-by-group (second date))]
         (str acc  "* "
              (first date)
              "\n"
              (reduce str "" groups))))
     "" dates)))

;;;;;;;;;;;;;;;;;
;;; serialize ;;;
;;;;;;;;;;;;;;;;;

(defn serialize-tasks
  "serialize a set of tasks

  the information serialized for
  each task includes the description
  and for the entire group the total is
  included.
  "
  [tasks]
  {"tasks" (map :description tasks)})

(defn tasks-by-group
  "serialize tasks by the group
  that they are a part of."
  [tasks]
  (let [groups (group-by :task-group tasks)]
    (map
     (fn [[group group-tasks]]
     {"group" group
      "tasks" (serialize-tasks group-tasks)})
    groups)))

(defn tasks-by-date-and-group
  "
  Serialize JSON for Tasks grouped
  by the date that they occur and
  the group that they done for.
  "
  [tasks]
  (let [dates (group-by :date tasks)]
    (str (json/write-str
          {"task-list" 
           (map
            (fn [[date date-tasks]]
              {"date" date
               "tasks" (tasks-by-group date-tasks)
               "time" (task/sum-tasks tasks)})
            dates)}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Serialize For DB File ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn serialize-task-to-file-format
  "Write task to string as db format"
  [task]
  (let [{:keys [start end description]} task]
    (str start " -- " end " * " description "\n")))

(defn -serialize-group-to-file-format
  "Write a group of tasks as a file
  assumes that all tasks in the list belong
  to the same group"
  [group tasks]
  (str "{"
       group
       "\n"
       (reduce
        (fn [acc task]
          (str acc (serialize-task-to-file-format task)))
        ""
        tasks)
       "}\n\n"))

(defn to-file-format
  "Create a string representation
  of a list of tasks to be written to disk
  "
  [tasks]
  (let [dates (->> tasks
                   (map :date)
                   distinct)]
    (when (not (= (count dates) 1))
      (throw
       (ex-info "Can only write tasks from a single date" {})))
    (let [groups (group-by :task-group tasks)]
      (reduce
       (fn [acc [group group-tasks]]
         (str acc (-serialize-group-to-file-format group group-tasks)))
       "" groups))))
