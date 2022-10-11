(ns arbitrage-bet.computer)

(defn calc-implied-probability [quote]
  (reduce (fn [vol quote]
            (+ (/ 1 quote) vol))
          0
          quote))

(defn collect-quote [match key]
  (try
    [(Float/parseFloat (.replace (key match) "," ".")) (:site match)]
    (catch Exception e (do (prn (str "Match invalid. Match : " (:match match)
                                     " | Site : " (:site match)))
                           [0.0 (:site match)]))))

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
    {:implied-proba (calc-implied-probability (map first [team1 draw team2]))
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
  (-> (filter not-eligible quotes)
      (calc-best-implied-probability)
      (sort-data)))