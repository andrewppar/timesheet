(ns timesheet.scratch)

(def generic-table (atom {}))

(defmacro defgeneric
  {:style/indent 1}
  [generic-name] ;; worry about arglists
  (swap! generic-table assoc (format "generic-fn-%s" generic-name) nil))

(defmacro generic-method
  {:style/indent 1}
  [generic-name args test-fn exec-fn] ;; coordinate args with fn
  (let [gen-name  (format "generic-fn-%s" generic-name)]
    (when-not (contains? (into #{} (keys @generic-table)) gen-name)
      (throw
       (ex-info "No generic for fn %s" generic-name
                {:caused-by "Undeclared generic"})))
    (let [new-test-fn `(try
                         (apply ~test-fn ~args)
                         (catch Exception _e# false))
          new-exec-fn `(try
                         (apply ~exec-fn ~args)
                         (catch Exception _e# false))]
      (if-let [gen-defn (get @generic-table gen-name)]
        (let [name      (second gen-defn)
              args      (nth gen-defn 2)
              cond-form (nth gen-defn 3)
              new-cond  (concat cond-form `(~new-test-fn ~new-exec-fn))
              new-defn  (list 'defn name args new-cond)]
          (swap! generic-table assoc gen-name new-defn)
          new-defn)
        (let [new-defn `(defn ~generic-name
                          ~args
                          (cond
                            ~new-test-fn ~new-exec-fn))]
          (swap! generic-table assoc gen-name new-defn)
          new-defn)))))

(defgeneric parse-time-string)

(generic-method parse-time-string
  [string]
  (fn [str]
    (let [[hours-string mins-string] (clojure.string/split str #":")
          hours (Integer/parseInt hours-string)
          mins  (Integer/parseInt mins-string)]
      (and (>= hours 0)
           (< hours 24)
           (>= mins 0)
           (< mins 60))))
  (fn [str]
    (java.time.LocalTime/parse str)))

(generic-method parse-time-string
  [string]
  (fn [string]
    (let [[month day _year] (map
                            (fn [str] (Integer/parseInt str))
                            (clojure.string/split string #"-"))]
      (and (>= month 1)
           (<= month 12)
           (>= day 1)
           (<= day 31))))
  (fn [string]
    (let [f (java.time.format.DateTimeFormatter/ofPattern "MM-dd-yyyy")]
      (java.time.LocalDate/parse string f))))

;;; Add a way to put in defaults and ensure they end up at the end of the cond
(generic-method parse-time-string ;; tag as default
  [string]
  (fn [_] true)
  (fn [string]
    (format "\"%s\" is not a time string")))

(parse-time-string "02-02-2222")
