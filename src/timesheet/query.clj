(ns timesheet.query
  (:require
   [timesheet.config   :as config]
   [timesheet.db.field :as field]
   [timesheet.db.index :as index]
   [timesheet.assert   :as assert]
   [timesheet.utils    :as utils])
  (:import
   (org.apache.lucene.document IntPoint)
   (org.apache.lucene.index Term)
   (org.apache.lucene.queryparser.classic QueryParser)
   (org.apache.lucene.search
    IndexSearcher BooleanClause$Occur
    BooleanQuery$Builder ScoreDoc TopDocs TermQuery)))

(def ^:private field-hierarchy  (atom (make-hierarchy)))

(swap! field-hierarchy derive :type :string)
(swap! field-hierarchy derive :subgroup :group)
(swap! field-hierarchy derive :supergroup :group)
(swap! field-hierarchy derive :group :string)
(swap! field-hierarchy derive :description :string)
(swap! field-hierarchy derive :start :time)
(swap! field-hierarchy derive :end :time)

(defmulti field-value
  {:arglists '([field-key field])}
  (fn [field-key _]
    (keyword field-key))
  :hierarchy field-hierarchy)

(defmethod field-value :string
  [_ field]
  (.stringValue field))

(defn ^:private maybe-add-zeros
  [string size]
  (let [string-size (count string)]
    (if (< string-size size)
      (let [difference (- size string-size)]
        (format "%s%s"
                (utils/make-string difference \0) string))
      string)))

(defmethod field-value :date
  [_ field]
  (let [raw-date-string  (format "%s" (.numericValue field))
        date-string      (maybe-add-zeros raw-date-string 8)
        year             (subs date-string 0 4)
        month            (subs date-string 4 6)
        day              (subs date-string 6 8)]
    (format "%s-%s-%s" month day year)))

(defmethod field-value :time
  [_ field]
  (let [raw-time-string (format "%s" (.numericValue field))
        time-string     (maybe-add-zeros raw-time-string 4)
        hour            (subs time-string 0 2)
        minute          (subs time-string 2 4)]
    (format "%s:%s" hour minute)))

;; TODO: Clean this up
(defn ^:private to-document
  "Convert a Lucene ScoreDoc object to a map"
  [^IndexSearcher searcher
   ^ScoreDoc score-doc]
  (let [doc-id (.doc score-doc)
        doc (.doc searcher doc-id)
        fields (.getFields doc)
        ;; Convert a document to a map
        ;; whose keys are field names
        ;; and whose values are vectors
        ;; of the values associated with that key.
        as-map (reduce (fn [acc ^org.apache.lucene.document.Field field]
                         (let [field-key  (keyword (.name field))
                               new-value  (field-value field-key field)]
                           (update acc field-key
                                   (fnil conj []) new-value)))
                       {:doc-id doc-id
                        :lucene-score (.score score-doc)} fields)]
    ;; If as-map has a vector with only one value
    ;; make that the sole value for key
    ;; TODO: Think about Maybe just always use vectors
    ;; and don't do this reduction...
    ;; Maybe we need to track labels we expect
    ;; to have multiple values...
    (reduce-kv (fn [result key value]
                 (if (and (vector? value)
                          (= (count (set value)) 1))

                   (assoc result key (first value))
                   (assoc result key value)))
               {} as-map)))

(defn ^:private score-search-results
  "Rank search results according to their lucene score"
  [^TopDocs search-results
   ^IndexSearcher searcher]
  (let [hits      (.scoreDocs search-results)
        hit-idxs  (->> hits count range)]
    (reduce (fn [acc doc-idx]
              (->> doc-idx
                   (nth hits)
                   (to-document searcher)
                   (conj acc)))
            [] hit-idxs)))

(defn ^:private taxonomy-query []
  (let [builder (BooleanQuery$Builder.)
        term    (TermQuery. (Term. "type" "taxonomy"))]
    (.add builder term BooleanClause$Occur/MUST)
    (.build builder)))

(defn taxonomy
  "Get all hierarchy statements in the database"
  []
  (let [searcher      (index/searcher)
        hits-per-page (config/get :hits-per-page)
        query         (taxonomy-query)]
    (when searcher
      (-> searcher
          (.search query hits-per-page)
          (score-search-results searcher)))))

(defn ^:private generate-query-date
  "Generate the lucene stored date from a date string"
  [date-string]
  (let [date-int (assert/parse-date date-string "-" "mdy")]
    (field/validate date-int :date)
    date-int))

(defn ^:private generate-task-query
  "Generate a query for tasks between `start-date` and `end-date`
  with any of `groups` and matching `description`."
  [groups start-date end-date description]
  (let [start          (generate-query-date start-date)
        end            (generate-query-date end-date)
        range-query    (IntPoint/newRangeQuery "date" start end)
        builder        (BooleanQuery$Builder.)
        group-builder  (BooleanQuery$Builder.)]
    (reduce
     (fn [acc group]
       (let [term-query (TermQuery. (Term. "group" group))]
         (.add acc term-query BooleanClause$Occur/SHOULD)))
     group-builder
     groups)
    (.add builder (.build group-builder) BooleanClause$Occur/MUST)
    (.add builder range-query BooleanClause$Occur/MUST)
    (when description
      (let [term (-> "description"
                     (QueryParser. (index/analyzer))
                     (.parse description))]
        (.add builder term BooleanClause$Occur/MUST)))
    (.build builder)))

(defn run
  "Run a query against the timesheet database"
  [groups start end description]
      ;;{start  "10-11-1987"
      ;; end    (today)}}]
  (let [query     (generate-task-query groups start end description)
         searcher  (index/searcher)
         hits      (config/get :hits-per-page)]
         (-> searcher
             (.search query hits)
             (score-search-results searcher))))
