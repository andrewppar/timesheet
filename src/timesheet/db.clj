(ns timesheet.db
  (:require
            [clojure.java.io :as io]
            [clojure.string :refer [split
                                    trim
                                    starts-with?
                                    join
                                    split-lines
                                    ]]
            [timesheet.globals :as global]
            [timesheet.serialize_tasks :as serialize]
            [timesheet.task :as task]
            [timesheet.time :as time]))

;; Database

(defn task-file
  "Given a date generates
  the path for the file corresponding to that date"
  [date]
  (str global/db-root "/" date))


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


(defn parse-dates
  "Generate all the tasks between the two given dates

  Takes a start date string an end date string, and a base directory
  and returns all the tasks contained in the database
  for the dates between the start and the end  inclusively
  "
  [start-date-string end-date-string base-directory]
  (let [start-date (time/new-date-from-string start-date-string)
        end-date   (time/new-date-from-string end-date-string)
        dates (time/dates-in-range start-date end-date)]
    (reduce
     (fn [acc date]
       (->> date
            (str base-directory "/")
            parse-task-file
            (concat acc)))
     [] dates)))

(defn -write-tasks
  "Wirte tasks to the database"
  [tasks path]
(with-open [w (clojure.java.io/writer path :overwrite true)]
      (.write w (serialize/to-file-format tasks))))

;; Adding Tasks

(defn add-task
  "Add a task item to the DB for
  a date, start, end, group, and
  description."
  [date start-time end-time group description]
  (let [filepath (task-file date)
        tasks    (parse-task-file filepath)
        new-task (task/new-task
                  start-time
                  end-time
                  date
                  group
                  description)
        updated-tasks (conj tasks new-task)]
    (-write-tasks updated-tasks filepath)))

;; Deleting Tasks

(defn delete-task
  "Removes a task from the database

  Looks for a task matching the day and time
  that its passed. If it finds one it deletes that task
  othwerwise it throws an error"
  [date start-time end-time]
  (let [filepath       (task-file date)
        tasks          (parse-task-file filepath)
        delete-start   (time/new-time-from-string start-time)
        delete-end     (time/new-time-from-string end-time)
        [removed-tasks
         new-tasks]    (reduce (fn [[removed-tasks new-tasks] task]
                                 (let [start (:start task)
                                       end   (:end task)]
                                   (if (and
                                        (= start delete-start)
                                        (= end   delete-end))
                                     [(conj removed-tasks task) new-tasks]
                                     [removed-tasks (conj new-tasks task)])))
         [[] []] tasks)]
    (if (= (count tasks) (count new-tasks))
      (throw
       (ex-info (str "Could not find a task to delete for "
                     date
                     " "
                     start-time
                     " "
                     end-time)
                {:causes #{:date date :start start-time :end end-time}}))
      (do
        (-write-tasks new-tasks filepath)
        (serialize/tasks-by-date-and-group removed-tasks)))))
