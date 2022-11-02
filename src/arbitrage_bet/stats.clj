(ns arbitrage-bet.stats)

(defn collect-unmatched-match []

  )

(defn get-number-of-match [data]
  (reduce
    (fn [results val]
      (let [size (count (->> (:result val)
                             :count-matches))]
        (if (nil? (get results size))
          (conj results (hash-map size 1))
          (conj results (hash-map size (+ 1 (get results size)))))))
    {}
    data)
  )

(defn print-%-matching [data]
  (prn "Matching result (Number of match) : ")
  (let [number-match (get-number-of-match data)
        total-size (count data)]
      (map #(let [[num-match num-count] %]
              (prn (str num-match " : "
                        num-count " quotes ("
                        (* (/ num-count total-size) 100))))
           number-match)))

(defn export-result [results]

  )

(defn get-stat-after-matching [data]
  (do
    (print-%-matching data)
    data))