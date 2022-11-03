(ns arbitrage-bet.preprocessing)

(defn filter-quote-no-data [data]
  (filter #(and (not (nil? (:quoteTeam1 %)))
                (not (nil? (:quoteTeam2 %)))
                (not (nil? (:quoteDraw %)))) data))