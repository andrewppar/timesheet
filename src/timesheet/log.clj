(ns timesheet.log
  (:require
   [clojure.java.io                :as io]
   [taoensso.timbre                :as timbre]
   [taoensso.timbre.appenders.core :as appenders]
   [timesheet.config               :as cfg]
   [timesheet.json                 :as json]))

(defn ^:private json-output [{:keys [level msg_ instant]}]
  (let [event (read-string (force msg_))]
    (json/encode {:event event :level level :time  instant})))

(defn init!
  ([]
   (let [log-level (cfg/get :log-level)]
     (init! log-level)))
  ([min-log-level]
   (when-let [log-dir (cfg/get :logging-location)]
     (let [io-log-dir (io/file log-dir)
           log-file (str log-dir "timesheet.log")]
       (when-not (.exists io-log-dir)
         (.mkdir io-log-dir))
       (timbre/merge-config!
        {:output-fn json-output
         :appenders
         {:spit (appenders/spit-appender
                 {:fname log-file})}})
       (timbre/set-min-level! min-log-level)
       (timbre/info {:type "logging"
                     :status "initialized"})))))


(defmulti log
  {:arglists '([level message])}
  (fn [level _]
    level))

(defmethod log :debug
  [_ message]
  (timbre/debug message))

(defmethod log :info
  [_ message]
  (timbre/info message))

(defmethod log :warn
  [_ message]
  (timbre/warn message))

(defmethod log :error
  [_ message]
  (timbre/error message))

(defmacro defn-logged [function docstring config args & body]
  (let [result-fn (get config :result-fn str)
        level     (get config :level :info)
        arg-vars  (reduce (fn [result arg]
                            (cond (and (not= arg '&)
                                       (symbol? arg)) (conj result arg)

                                  (map? arg)
                                  (concat result (get arg :keys))

                                  :else result))
                          [] args)
        arg-map   (zipmap (map keyword arg-vars) arg-vars)
        fn-name   (name function)]
    `(defn ~function ~docstring ~args
       (let [id# (random-uuid)]
         (log ~level {:id id#
                      :status "starting"
                      :type ~fn-name
                      :args (update-vals ~arg-map str)})
         (let [result# (do ~@body)]
           (log ~level {:id id# :status "complete" :result (~result-fn result#)})
           result#)))))
