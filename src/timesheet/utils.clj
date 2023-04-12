(ns timesheet.utils
  (:require
   [clojure.string :as string]))

(defn make-string
  ([char]
   (make-string char 1))
  ([n char]
   (string/join "" (repeat n char))))
