(ns timesheet.db)

(def month-threshold-map
  "A map of months to the number of days in them"
  {(keyword (str 1)), 31
   (keyword (str 2)), 28
(keyword (str 3)), 31
(keyword (str 4)), 30
(keyword (str 5)), 31
(keyword (str 6)), 30
(keyword (str 7)), 31
(keyword (str 8)), 31
(keyword (str 9)), 30
(keyword (str 10)), 31
(keyword (str 11)), 30
(keyword (str 12)), 31})

(defn week-internal [month day]
  "Given a month and day generate the next
   seven days"
  (map
   (fn [offset]
     (let [month-threshold ((keyword (str month)) month-threshold-map)
           new-month    (if (> (+ offset day) month-threshold)
                          (+ month 1)
                          month)
           new-day      (if (> (+ day offset) month-threshold)
                          (- (+ day offset) month-threshold)
                          (+ day offset))
           month-string (if (> 10 new-month) (str "0" new-month) (str new-month))
           day-string   (if (> 10 new-day) (str "0" new-day) (str new-day))]
       (format "%s-%s-2021" month-string day-string)))
   [0 1 2 3 4 5 6]))
