(ns arbitrage-bet.computer)

(defn calc-implied-probability [quotes]
  (reduce (fn [vol quote]
            (+ (/ 1 quote) vol))
          0
          quotes))

(defn safe-calc-implied-probability [team1 draw team2]
  (try (calc-implied-probability (map first [team1 draw team2]))
       (catch Exception e (do (prn (str "Issue computing quote for : "
                                        (first team1) " / " (second team1)
                                        ", " (first draw) " / " (second draw)
                                        ", " (first team2) " / " (second team2)))
                              100))))

(defn collect-quote [match key]
    [(key match) (:site match)])

(defn pick-best [array-quote]
  (reduce
    (fn [best x]
      (if (> (first x) (first best))
        x
        best))
    [0 "no site"]
    array-quote))

(defn best-by-outcome [matches]
  (let [team1 (map #(collect-quote % :quoteTeam1) matches)
        team2 (map #(collect-quote % :quoteTeam2) matches)
        draw  (map #(collect-quote % :quoteDraw) matches)]
    (map pick-best [team1 draw team2])))

(defn compute-quote [quote]
  (let [[team1 draw team2] (best-by-outcome quote)]
    {:implied-proba (safe-calc-implied-probability team1 draw team2)
     :team1 team1
     :team2 team2
     :draw draw
     :count-matches quote}))

(defn calc-best-implied-probability [quotes]
  (reduce
    (fn [v x]
      (conj v {:name   (:name x)
               :date   (:date x)
               :result (compute-quote (:quotes x))}))
    []
    quotes))

(defn not-eligible [quote]
  (let [count-matches (count (:quotes quote))]
    (and (> count-matches 1)
         (<= count-matches 5))))

(defn exploitable [data]
  (< (-> data
         :result
         :implied-proba)
     1))

(defn sort-data [datas]
  (reduce
    (fn [result x]
      (if (exploitable x)
        (conj result {:exploitable (conj (:exploitable result) x)})
        (conj result {:not-exploitable (conj (:not-exploitable result) x)})))
    {:exploitable []
     :not-exploitable []}
    datas))

(defn compute-quotes [quotes]
  (calc-best-implied-probability quotes))