(ns timesheet.db.transact
  (:require [timesheet.db.index :as index])
  (:import (org.apache.lucene.search Query)))

(defn operations-running?
  "Check if there are still ongoing document add operations filtering
  out competed operations" [operation-atom]
  (swap! operation-atom
         (fn [ops] (filter
                   (fn [future]
                     (not (future-done? future)))
                   ops)))
  (boolean (seq @operation-atom)))

(defn commit!
  "Commit the pending writes to the index"
  [writer operation-atom]
  (while (operations-running? operation-atom)
    (Thread/sleep 1000))
  (.commit writer))

(defn close!
  "Close the current writer"
  [writer]
  (.close writer))

(defmacro with-operations [writer operation-atom & body]
  `(let [~operation-atom (atom [])
         ~writer         (index/writer)]
     (let [error# (try
                    (do ~@body)
                    (catch Exception e# e#))]
       (commit! ~writer ~operation-atom)
       (close! ~writer)
       (if (isa? (type error#) java.lang.Exception)
         (throw error#)
         error#))))

(defn add!
  [documents]
  (with-operations writer ops
    (reduce
     (fn [_ document]
       (let [process (future (.addDocument writer document))]
         (swap! ops conj process)))
     writer
     documents)))

(defn delete!
  "Delete documents from the index."
  [queries]
  (with-operations writer ops
    (let [arr (make-array Query (count queries))]
      (reduce
       (fn [_ idx] (aset arr idx (nth queries idx)))
       arr
       (range (count queries)))
      (let [process (future (.deleteDocuments writer arr))]
        (swap! ops conj process)))))
