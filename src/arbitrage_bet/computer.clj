(ns arbitrage-bet.computer)

(defn calc-implied-probability [quote]
  (reduce (fn [vol quote]
            (+ (/ 1 quote) vol))
          0
          quote))



(defn compute-quotes)