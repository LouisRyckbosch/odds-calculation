(ns arbitrage-bet.new-matching-process
  (:require [java-time.api :as jt]
            [arbitrage-bet.levenshtein :as l]))

(defn gen-id [quote]
  (str (:match quote) "|" (:site quote)))

(defn calc-ratio [x y]
  (if (> x y)
    (/ x y)
    (/ y x)))

(defn collect-quote [quote]
  {:quoteTeam1 (:quoteTeam1 quote)
   :quoteTeam2 (:quoteTeam2 quote)
   :quoteDraw  (:quoteDraw quote)}
  )

(defn quote-too-good? [match quote]
  (let [match-quote (collect-quote match)
        result (reduce
                 (fn [x [key value]]
                   (+ (calc-ratio (key quote) value) x))
                 0
                 match-quote)]
    (< 4.5 result)))

(defn link-if-best [quote quote2]
  (if (or (= (:site quote) (:site quote2))
          (quote-too-good? quote quote2))
    quote
    (let [leven-score (l/levenshtein (:match quote) (:match quote2))
          previous-score (:leven-score quote)]
      (if (> previous-score leven-score)
        (conj quote {:leven-score leven-score
                     :best-match  (gen-id quote2)})
        quote))))

(defn add-closest-match [quote quotes]
  (let [quote (conj quote {:leven-score 999
                           :best-match  :no-match})]
    (reduce link-if-best
            quote
            quotes)))

(defn link-quotes-single-day [quotes]
  (reduce (fn [result quote]
            (conj result (add-closest-match quote quotes)))
          []
          quotes))

(defn link-quotes-by-name [quotes]
  (reduce
    (fn [col quote-for-one-day]
      (conj col (link-quotes-single-day quote-for-one-day)))
    []
    (val quotes)))

(defn trunc-time-from-date [quote]
  (let [[y m d] (java-time.core/as quote :year :month-of-year :day-of-month)]
    (keyword (.toString (java-time.api/local-date-time y m d 0 0)))))

(defn add-quote
  ([result-map key quote]
   (conj result-map {key (conj (get result-map key) quote)}))
  ([match quote]
   (conj match {:quotes (conj (:quotes match) quote)})))

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

(defn gen-name [quote]
  (str (:nameTeam1 quote) " - " (:nameTeam2 quote)))

(defn create-match [quote]
  {:date             (:date quote)
   :name             (gen-name quote)
   :nameTeam1        (:nameTeam1 quote)
   :nameTeam2        (:nameTeam2 quote)
   :quotes           [quote]})

(defn add-to-no-match [quote result]
  (let [undefined (first (filter #(= (:match %) :undefined) result))]
    (if (nil? undefined)
      (conj (create-match quote) {:name :undefined})
      (add-quote undefined quote))))

(defn contains-quote? [match quote]
  (let [quotes (:quotes match)]
    (-> (filter #(= (gen-id %) (gen-id quote)) quotes)
        (first))))

(defn find-group [result quote]
  (first (filter #(contains-quote? % quote) result)))

(defn add-to-results [quote match result]
  (let [match-group (find-group result match)]
    (if (nil? match-group)
      (-> (create-match match)
          (add-quote quote))
      (add-quote match-group quote))))

(defn regroup-quote [quote quotes results]
  (let [match (first (filter #(match-with-quote quote) quotes))]
    (if (nil? match)
      (add-to-no-match quote results)
      (add-to-results quote match result))))

(defn regroup-quotes-by-links [quotes]
  (reduce
    (fn [col ]))
  )

(defn filter-nil-date [quotes]
  (filter #(and (not (nil? (:date %)))
                (not (= (:date %) :undefined))) quotes))

(defn matching-process-with-info [quotes]
  (-> (filter-nil-date quotes)
      (map-quote-per-day)
      (link-quotes-by-name)
      (regroup-quotes-by-link)))
