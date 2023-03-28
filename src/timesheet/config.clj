(ns timesheet.config
  (:require [omniconf.core :as cfg]))

(cfg/define
  {:index-location {:type :string
                    :default "/opt/timesheet"
                    :required true
                    :description
                    "The location on disk where the lucene's index
                     is stored"}
   :logging-location {:type :string
                      :default "/var/log/"
                      :required false}
   :log-level      {:type :keyword
                    :default :info
                    :required true}
   :hits-per-page  {:required true
                    :default 500
                    :description
                    "The default number of lucene documents to return
                     in a query"}
   :taxonomy-location {:required true
                       :default "resources/groups.edn"
                       :description "The default location of the group
                        hierarchy."}
   :legacy-location   {:required false
                       :description
                       "The location of any legacy timesheet database"}})

(defn init! []
  (cfg/populate-from-env)
  (cfg/verify))

#_{:clj-kondo/ignore [:redefined-var]}
(defn get [& args]
  (apply cfg/get args))

(comment

  (init!)

  )
