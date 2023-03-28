(ns timesheet.taxonomy
  (:require
   [clojure.edn           :as edn]
   [loom.graph            :as graph]
   [loom.alg-generic      :as alg]
   [timesheet.assert      :as assert]
   [timesheet.config      :as cfg]
   [timesheet.query       :as query]))


(defmulti parse-taxonomy
  {:arglists '([spec parent accumulator])}
  (fn [spec _ _]
    (type spec)))

(defmethod parse-taxonomy clojure.lang.PersistentArrayMap
  [spec parent accumulator]
  (let [middle      (keys spec)
        updated-acc (if (nil? parent)
                      accumulator
                      (->> middle
                           (map (fn [subgroup] [subgroup parent]))
                           (concat accumulator)))]
    (reduce
     (fn [acc supergroup]
       (parse-taxonomy (get spec supergroup) supergroup acc))
     updated-acc
     middle)))

(defmethod parse-taxonomy clojure.lang.PersistentVector
  [spec parent accumulator]
  (reduce
   (fn [acc item] (parse-taxonomy item parent acc))
   accumulator
   spec))

(defmethod parse-taxonomy java.lang.String
  [spec parent accumulator]
  (conj accumulator [spec parent]))

(defn insert-taxonomy!
  "Given the taxonomy configuration specified in the timesheet.config
  generate a taxonomy that can be inserted into timesheet's taxonomy index."
  []
  (let [loc (cfg/get :taxonomy-location)
        raw-taxonomy (assoc {} "Group" (edn/read-string (slurp loc)))
        to-assert (parse-taxonomy raw-taxonomy nil #{})]
    (apply assert/assert! to-assert)))

(defn group-taxonomy
  "Generate an in memory representation of the taxonomy of groups
   that has been entered into the "
  []
  (let [all-groups (query/taxonomy)
        graph      (graph/digraph)]
    (reduce
     (fn [acc assert]
       (let [{:keys [subgroup supergroup]} assert]
         (graph/add-edges acc [subgroup supergroup])))
     graph all-groups)))

(defn groups
  "Get all groups in the taxonomy"
  [graph]
  (graph/nodes graph))

(defn subgroups
  "Get all `subgroups` of `group` in `graph`."
  [graph group]
  (alg/pre-traverse
   (graph/predecessors graph)
   group))
