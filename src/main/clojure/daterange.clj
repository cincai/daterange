(ns daterange)

(defn is-leap [y] 
  (if (= (mod y 4) 0)
    (if (or (> (mod y 100) 0) (= (mod y 400) 0))
      true false) false))

(defn days-in-month [y m]
  (let [thirties #{4 6 9 11}]
    (if (= 2 m) 
      (if (is-leap y) 29 28)
      (if (contains? thirties m) 30 31))))

(defn is-valid-date [y m d]
  (when-not (> y 0)
    (throw (IllegalArgumentException. "Year must be greater than 0")))
  (when-not (> m 0)
    (throw (IllegalArgumentException. "Month must be greater than 0")))
  (when-not (<= m 12)
    (throw (IllegalArgumentException. "Month must be less or equal 12")))
  (when-not (> d 0)
    (throw (IllegalArgumentException. "Day must be greater than 0")))
  (when-not (<= d (days-in-month y m))
    (throw (IllegalArgumentException. "Day is invalid"))))

(defn leap-years-past [y]
  (let [years-past (- y 1)]
    (+ (quot years-past 4) (quot years-past -100) (quot years-past 400))))

(defn rank [y m d]
  (is-valid-date y m d)
  (let [days-to-start-of-year (+ (* (- y 1) 365) (leap-years-past y))
        days-from-start-of-year (reduce + d (map #(days-in-month y %) (range 1 m)))]
    (+ days-to-start-of-year days-from-start-of-year)))
     
(defn rank-by-month [y m d]
  (is-valid-date y m d)
  (let [years-past (- y 1)
        full-months-past (+ (* 12 years-past) m -1)]
    (+ full-months-past (/ d (days-in-month y m)))))

(defn days-between [start end]
  (let [[y1 m1 d1] start
        [y2 m2 d2] end]
    (- (rank y2 m2 d2) (rank y1 m1 d1))))

(defn months-between [start end]
  (let [[y1 m1 d1] start
        [y2 m2 d2] end]
    (- (rank-by-month y2 m2 d2) (rank-by-month y1 m1 d1))))
