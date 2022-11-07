(ns arbitrage-bet.new-matching-process
  (:require [java-time.api :as jt]
            [arbitrage-bet.levenshtein :as l]))

(defn gen-id [quote]
  (str (:match quote) "|" (:site quote)))

(defn link-if-best [quote quote2]
  (let [leven-score (l/levenshtein quote quote2)
        previous-score (:leven-score quote)]
    (if (< previous-score leven-score)
      (conj quote {:leven-score leven-score
                   :best-match  (gen-id quote2)}))))

(defn add-closest-match [quote quotes]
  (let [quote (conj quote {:leven-score 999
                           :best-match  :no-match})]
    (reduce link-if-best
            quote
            quotes)))

(defn match-quotes-between-them [quotes]
  (reduce (fn [result quote]
            (conj result (add-closest-match quote quotes)))
          []
          quotes))

(defn trunc-time-from-date [quote]
  (let [[y m d] (java-time.core/as quote :year :month-of-year :day-of-month)]
    (java-time.api/local-date-time y m d 0 0)))

(defn add-quote [result-map key quote]
  (conj result-map {key (conj (get result-map key) quote)}))

(defn put-to-quote-map [result-map quote]
  (let [date-trunc (trunc-time-from-date (:date quote))
        values (get result-map date-trunc)]
    (if (nil? values)
      (conj result-map {date-trunc [quote]})
      (add-quote result-map date-trunc quote))))

(defn map-quote-per-day [quotes]
  (reduce put-to-quote-map
          {}
          quotes))

(defn filter-nil-date [quotes]
  (filter #(and (not (nil? (:date %)))
                (not (= (:date %) :undefined))) quotes))

(defn test [quotes]
  (match-quotes-between-them (second quotes)))

(defn matching-process-with-info [quotes]
  (-> (filter-nil-date quotes)
      (map-quote-per-day)
      (test)))
