(ns timesheet.assert
  (:require
   [clojure.string        :as string]
   [timesheet.db.transact :as transact]
   [timesheet.db.document :as document])
  (:import
   (org.apache.lucene.document IntPoint)
   (org.apache.lucene.index Term)
   (org.apache.lucene.search
    BooleanClause$Occur BooleanQuery$Builder TermQuery)))

;; TODO: Should any of this be moved to time.clj
(defmulti ^:private parse-time
  {:arglists '([string separator])}
  (fn [_ separator]
    separator))

(defmethod ^:private parse-time ":"
  [time-string _]
  (let [[hours minutes] (string/split (string/trim time-string) #":")]
    (Integer/parseInt (format "%s%s" hours minutes))))

(defmulti parse-date
  {:arglists '([string separator order])}
  (fn [_ separator order]
    [separator order]))

(defmethod parse-date ["-" "mdy"]
  [date-string _ _]
  (let [[month day year] (string/split (string/trim date-string) #"-")]
    (Integer/parseInt (format "%s%s%s" year month day))))

(defmulti ^:private document
  {:arglists '([args])}
  (fn [args]
    (count args)))

(defmethod ^:private document 5
  [args]
  (let [[date-string start-string end-string group description] args
        date     (parse-date date-string "-" "mdy")
        start    (parse-time start-string ":")
        end      (parse-time end-string ":")
        document (document/task date start end group description)]
    (when (not (< start end))
      (throw
       (ex-info
        (format "Task start %s and end %s specify a negative time range"
                start-string end-string)
        {:cause `(not (< ~start ~end))})))
    document))

(defmethod ^:private document 2
  [args]
  (let [[subgroup supergroup] args]
    (document/taxonomy subgroup supergroup)))

(defn assert!
  "Add new assertions for the assertion specifications."
  [& asserts]
  (->> asserts
       (map document)
       transact/add!))

(defmulti delete-document-query
  "Generate a query to delete documents from the index."
  {:arglists '([args])}
  (fn [args]
    (count args)))


(defmethod delete-document-query 3
  [args]
  (let [[date start end] args
        builder          (BooleanQuery$Builder.)
        date-int         (parse-date date "-" "mdy")
        date-term        (IntPoint/newExactQuery "date" date-int)
        start-int        (parse-time start ":")
        start-term       (IntPoint/newExactQuery "start" start-int)
        end-int          (parse-time end ":")
        end-term         (IntPoint/newExactQuery "end" end-int)
        type-term        (TermQuery. (Term. "type" "task"))]
    (.add builder type-term  BooleanClause$Occur/MUST)
    (.add builder date-term BooleanClause$Occur/MUST)
    (.add builder start-term  BooleanClause$Occur/MUST)
    (.add builder end-term BooleanClause$Occur/MUST)
    (.build builder)))

(defmethod delete-document-query 2
  [args]
  (let [[subgroup supergroup] args
        builder (BooleanQuery$Builder.)]
    (letfn [(new-term [term value]
              (TermQuery. (Term. term value)))]
      (.add builder (new-term "type" "taxonomy") BooleanClause$Occur/MUST)
      (.add builder (new-term "subgroup" subgroup) BooleanClause$Occur/MUST)
      (.add builder (new-term "supergroup" supergroup) BooleanClause$Occur/MUST))
    (.build builder)))

(defn retract! [& specs]
  (->> specs
       (map (fn [spec] (delete-document-query spec)))
       transact/delete!))

(comment
  (assert! ["Klezmer Archives" "Consulting"]
           ["Job Search" "Personal"]
           ["Avicenna" "Data Products"]
           ["Data Products" "Reify"])



  (assert! ["10-11-1987" "10:00" "11:00" "Personal" "Get Born"]
           ["02-22-2023" "12:00" "12:30" "Cisco"    "Standup"]
           ["02-23-2023" "12:00" "12:30" "Cisco"    "Standup"]
           ["02-23-2023" "21:00" "22:00" "Personal" "Work on timesheet software"])

  (assert! ["02-21-2023" "21:00" "22:00" "Personal" "Call Doug"])
  (assert! ["02-21-2023" "09:00" "09:30" "Cisco" "Set up agenda for the day"])
  (assert! ["02-21-2023" "12:00" "12:30" "Cisco" "Stand up"])

  )
