(ns timesheet.test-config
  (:require
   [clojure.java.io    :as io]
   [clojure.walk       :as walk]
   [omniconf.core      :as cfg]
   [timesheet.config   :as config]
   [timesheet.taxonomy :as tax]
   [timesheet.state    :as state]))

(defn wrap-setup
  [test-fn]
  (cfg/populate-from-map
   {:index-location "test/timesheet/test_assets/timesheet"
    :hits-per-page  5
    :taxonomy-location "test/timesheet/test_assets/test-groups.edn"})
  (tax/insert-taxonomy!)
  (state/init!)
  (test-fn)
  (state/stop!)
  (let [index (cfg/get :index-location)]
    (->> index
         io/file
         file-seq
         (remove (fn [file] (= (str file) index)))
         (mapv io/delete-file)))
  (cfg/populate-from-map
   {:index-location nil
    :hits-per-page  nil
    :taxonomy-location nil}))

(defn car [item]
  (first item))

(defn cdr [item]
  (rest item))

(defn cadr [item]
  (-> item cdr car))

(defmulti substitute-in-sexp
  {:arglists '([sexp sub-map])}
  (fn [sexp _]
    (type sexp)))

(defmethod substitute-in-sexp clojure.lang.Symbol
  [sym sub-map]
  (get sub-map sym sym))

(defmethod substitute-in-sexp java.lang.String
  [string sub-map]
  (get sub-map string string))

(defmethod substitute-in-sexp clojure.lang.PersistentVector
  [vector-sexp sub-map]
  (if-let [res (get sub-map vector-sexp)]
    res
    (into []
          (map
           (fn [item] (substitute-in-sexp item sub-map))
           vector-sexp))))

(defmethod substitute-in-sexp clojure.lang.PersistentList
  [list-sexp sub-map]
  (if-let [res (get sub-map list-sexp)]
    res
    (map
     (fn [item] (substitute-in-sexp item sub-map))
     list-sexp)))

(defmethod substitute-in-sexp clojure.lang.PersistentArrayMap
  [map-sexp sub-map]
  (if-let [res (get sub-map map-sexp)]
    res
    (reduce-kv
     (fn [acc k v]
       (let [new-k (substitute-in-sexp sub-map k)
             new-v (substitute-in-sexp sub-map v)]
         (assoc acc new-k new-v)))
     {} map-sexp)))

(defn nth-are-case
  [form n]
  (let [args    (nth form 1)
        case-fn (nth form 2)
        cases   (partition (count args) (drop 3 form))
        test-case (nth cases n)
        case-map  (zipmap args test-case)]
    (substitute-in-sexp case-fn case-map)))
