(ns timesheet.db.index
  (:require
   [clojure.java.io :as io]
   [timesheet.config :as config])
  (:import
   (org.apache.lucene.analysis.standard StandardAnalyzer)
   (org.apache.lucene.index DirectoryReader IndexWriter IndexWriterConfig)
   (org.apache.lucene.search IndexSearcher)
   (org.apache.lucene.store NIOFSDirectory FSDirectory)))

(defn analyzer
  "Create a new standard analyzer"
  []
  (StandardAnalyzer.))

;;Depricated for now
(defn store
  "Access the lucene store on disk"
  [location]
  (let [path (.toPath (io/file (format "%s" location)))]
    (NIOFSDirectory. path)))

(defn open-directory
  "Open a FSDirectory assicated with the index location."
  []
  (-> (config/get :index-location)
      (format "%s")
      io/file
      .toPath
      FSDirectory/open))

(defn writer
  "Create an index writer for lucene using `analyzer` for `location`"
  []
  (let [analyzer  (analyzer)
        config    (IndexWriterConfig. analyzer)
        index-dir (open-directory)]
    (IndexWriter. index-dir config)))

(defn searcher
  "Create a new lucene index searcher for the passed store."
  []
  (let [reader (DirectoryReader/open (open-directory))]
  (IndexSearcher. reader)))
