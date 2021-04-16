(ns timesheet.core
  (:gen-class)
  (:require [clojure.string :refer [split
                                    trim
                                    starts-with?
                                    join
                                    split-lines
                                    ]]
            [clojure.java.io :as io]
            [timesheet.task :as task]
            [timesheet.serialize_tasks :as serialize]))


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

(defn -main
  ([dates]
   (-main dates true))
  ([dates truncate-descriptions?]
   (let [root  "/Users/andrew/Documents/Records_flat"
         filepaths (map (fn [date] (str root "/" date)) dates)
         tasks     (reduce (fn [acc filepath]
                             (concat acc (parse-task-file filepath)))
                           [] filepaths)]
     (serialize/tabular-show-tasks tasks truncate-descriptions?))))

(comment
  (do
    (require '[clojure.tools.namespace.repl :refer [refresh]])
    (refresh))
  )
