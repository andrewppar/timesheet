(ns timesheet.json
  (:require [jsonista.core :as json]))

(defn encode
  "Encode a clojure map as a json string."
  [object]
  (json/write-value-as-string object))

(defn decode
  "Decode a string representing json."
  ([string]
   (decode string true))
  ([string use-keyword?]
   (let [mapper (json/object-mapper
                 {:decode-key-fn use-keyword?})]
     (json/read-value string mapper))))
