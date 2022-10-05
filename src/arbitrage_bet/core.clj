(ns arbitrage-bet.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(def quote
  [[:1 "1.37 5 8.30"]
   [:2 "1.37 5 8.50"]
   [:4 "1.31 3.90 6"]
   [:5 "1.37 5 8.50"]
   [:6 "1.37 4.80 8.25"]
   [:7 "1.37 5.05 7.75"]
   [:8 "1.40 4.90 8.25"]
   [:9 "1.37 4.50 7.70"]
   [:10 "1.30 3.80 5.75"]
   [:11 "1.38 5 7.75"]
   [:12 "1.36 4.90 8.00"]])

(defn parse-quote [to-parse]
  (->> (clojure.string/split to-parse #" ")
       (map #(Float/parseFloat %))))

(defn format-quote [quote]
  (let [parsed-quote (parse-quote (second quote))
        site-name (first quote)]
    (map #(vector site-name %) parsed-quote)))

(defn parse-quotes [quotes]
  (map
    format-quote
    quotes))

(defn calc-implied-probability [quote]
  (reduce (fn [vol quote]
            (+ (/ 1 quote) vol))
          0
          quote))

(defn calc-implied-probability-from-quote [quote]
  (calc-implied-probability (map second quote)))

(defn quote-by-outcome [outcomes quote]
  (vector (conj (first outcomes) (first quote))
          (conj (second outcomes) (second quote))
          (conj (last outcomes) (last quote))))

(defn order-quotes [list]
  (reduce quote-by-outcome
          '[[] [] []]
          list))

(defn find-max [quotes-by-outcome]
  (reduce
    (fn [result quote]
      (if (> (second result) (second quote))
        result
        quote))
    [:none 0]
    quotes-by-outcome))

(defn get-max-from-all-sites [outcomes]
  (let [values (map find-max outcomes)]
    (println "Best values: " values " \\n"
             "Implied volatility: " (calc-implied-probability-from-quote values))))