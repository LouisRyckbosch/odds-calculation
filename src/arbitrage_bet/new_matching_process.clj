(ns arbitrage-bet.new-matching-process
  (:require [java-time.api :as jt]))

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

(defn matching-process-with-info [quotes]
  (-> (filter-nil-date quotes)
      (map-quote-per-day)))
