(ns clustering-mmd.core)

(defn dist [[x y] [a b]]
  (let [dx (- x a)
        dy (- y b)]
    (Math/sqrt (+ (* dx dx) (* dy dy)))))

(defn ->prn [x] (prn x) x)

(defn ->pprn [x]
  (clojure.pprint/pprint x)
  x)

(defn assign-cluster [clusters point]
  (let [cluster (->> (keys clusters)
                  (map (juxt (partial dist point) identity))
                  (sort-by first)
                  first
                  second)]
    (update-in clusters [cluster] conj point)))

(defn centroid [points]
  (when (pos? (count points))
    (let [xs (->> points (map first) (reduce +))
          ys (->> points (map second) (reduce +))
          n  (count points)]
      (->> [(/ xs n) (/ ys n)]
        (mapv float)))))

(defn compute-centroids [clusters]
  (reduce (fn [centroids [c pts]]
            (if-let [c (centroid pts)] 
              (conj centroids c)
              centroids))
    #{}
    clusters))

(defn k-means [data centroids n]
  (prn :iteration n)
  (let [clusters  (zipmap centroids (repeat #{}))
        clusters (reduce assign-cluster clusters data)
        _ (->pprn clusters)
        centroids (compute-centroids clusters)]
    ;;(->pprn centroids)
    (prn :------------------)
    (if (pos? n)
      (recur data centroids (dec n))
      clusters)))

(def data [[25 125]
           [44 105]
           [29 97]
           [35 63]
           [55 63]
           [42 57]
           [23 40]
           [64 37]
           [33 22]
           [55 20]
           [28 145]
           [65 140]
           [50 130]
           [38 115]
           [55 118]
           [50 90]
           [43 83]
           [63 88]
           [50 60]
           [50 30]])

(def centroids [[25 125]
                [44 105]
                [29 97]
                [35 63]
                [55 63]
                [42 57]
                [23 40]
                [64 37]
                [33 22]
                [55 20]])

(clojure.pprint/pprint
  (k-means data centroids 1))

(def squares [{:y [[3 3] [10,1]]
               :b [[13 10] [16 4]]}
              {:y [[6 15] [13 7]]
               :b [[16 16] [18 5]]}
              {:y [[6 7] [11 4]]
               :b [[14 10] [23 6]]}
              {:y [[3 15] [13 7]]
               :b [[14 10] [23 6]]}])

(defn check-dist [{:keys [y b]}]
  (let [[yul ylr] y
        [bul blr] b
        ab        [5 10]
        cd        [20 5]]
    (and (< (dist yul ab) (dist yul cd))
      (< (dist ylr ab) (dist ylr cd))
      (> (dist bul ab) (dist bul cd))
      (> (dist blr ab) (dist blr cd)))))

(filter check-dist squares)

(def points #{[1 6] [3 7] [4 3] [7 7] [8 2] [9 5]})
(def pts-names {[1 6] :a 
                [3 7] :b
                [4 3] :c
                [7 7] :d
                [8 2] :e
                [9 5] :f})

(defn add-point [repr points]
  (if (seq points)
    (let [distv  (fn distv [a b]
                   [(dist a b) a])
          minmax (->> points
                   (map #(->> repr
                          (map (partial distv %))
                          (sort-by first)
                          first))
                   (sort-by first)
                   reverse
                   first
                   second)]
      
      (recur (conj repr minmax) (disj points minmax)))
    repr))

(map pts-names
  (add-point [[0 0] [10 10]] points))

