(ns arbitrage-bet.matching-process
  (:require [arbitrage-bet.levenshtein :as l]
            [java-time.api :as jt]))

(defn same-day [match quote]
  (if (or (= (:date match) :undefined) (= (:date quote) :undefined))
    true
    (= (jt/as (:date match) :year :month-of-year :day-of-month)
       (jt/as (:date quote) :year :month-of-year :day-of-month))))

(defn limit [string]
  (* (.length string) 1.4))

(defn teams-name-minimal-match? [match quote]
  (and (>= (limit (:nameTeam1 quote)) (l/levenshtein (:nameTeam1 match) (:nameTeam1 quote)))
       (>= (limit (:nameTeam2 quote)) (l/levenshtein (:nameTeam2 match) (:nameTeam2 quote)))))

(defn calc-ratio [x y]
  (if (> x y)
    (/ x y)
    (/ y x)))

(defn check-quote-not-too-good [match quote]
  (let [match-quote (:quote-main-match match)
        result (reduce
                 (fn [x [key value]]
                   (+ (calc-ratio (key quote) value) x))
                 0
                 match-quote)]
    (> 4.5 result)))

(defn not-excluded? [leven match quote]
  (and (>= (/ (.length (:name match)) 2) leven)
       (same-day match quote)
       (teams-name-minimal-match? match quote)
       (check-quote-not-too-good match quote)))

(defn gen-name [quote]
  (str (:nameTeam1 quote) " - " (:nameTeam2 quote)))

(defn add-leven [quote leven]
  (conj quote {:leven-score leven}))

(defn collect-quote [quote]
  {:quoteTeam1 (:quoteTeam1 quote)
   :quoteTeam2 (:quoteTeam2 quote)
   :quoteDraw  (:quoteDraw quote)}
  )

(defn create-match [quote]
  {:date             (:date quote)
   :name             (gen-name quote)
   :nameTeam1        (:nameTeam1 quote)
   :nameTeam2        (:nameTeam2 quote)
   :quote-main-match (collect-quote quote)
   :quotes           [(add-leven quote 0)]})

(defn add-quote [match quote leven]
  (let [quotes (:quotes match)
        quotes-u (conj quotes (add-leven quote leven))]
    (conj match {:quotes quotes-u})))

(defn create-and-add [vec quote]
  (conj vec (create-match quote)))

(defn quote-from-site [match quote]
  (->> (:quotes match)
       (filter #(= (:site quote) (:site %)))))

(defn already-have-quote-from-site? [match quote]
  (->> (quote-from-site match quote)
       empty?
       not))

(defn replace-in-matches [matches match]
  (-> (filter #(not= (:name match) (:name %)) matches)
      (conj match)))

(defn replace-in-v-struct [v match]
  {:data   (replace-in-matches (:data v) match)
   :added? true})

(defn conj-in-v-struct [v match]
  {:data   (conj (:data v) match)
   :added? (:added? v)})

(defn replace-quote [v match quote leven]
  (let [quotes (filter #(not= (:site quote) (:site %)) (:quotes match))
        match-u (->> (conj quotes (add-leven quote leven))
                     (hash-map :quotes)
                     (conj match))]
    (replace-in-v-struct v match-u)))

(defn add-if-best [v match quote leven]
  (let [previous-quote (first (quote-from-site match quote))
        previous-score (:leven-score previous-quote)]
    (if (< leven previous-score)
      (replace-quote v match quote leven)
      (conj-in-v-struct v match))))

(defn update-or-replace-if-best [v match quote leven]
  (if (already-have-quote-from-site? match quote)
    (add-if-best v match quote leven)
    (replace-in-v-struct v (add-quote match quote leven))))

(defn add-if-best-match [v match quote]
  (let [leven (l/levenshtein (:name match) (gen-name quote))]
    (if (not-excluded? leven match quote)
      (update-or-replace-if-best v match quote leven)
      (conj-in-v-struct v match))))

(defn add-quote-to-vec [v quote]
  (let [v-quote-added (reduce (fn [v-up match]
                                (add-if-best-match v-up match quote))
                              {:data   []
                               :added? false}
                              v)]
    (if (not (:added? v-quote-added))
      (create-and-add (:data v-quote-added) quote)
      (:data v-quote-added))))

(defn gen-index-to-log [v]
  (map #(int (/ (* (.length v) %) 20)) (range 20)))

(defn log-progress [index indexes]
  (if (.contains indexes index)
    (prn (str "Matching process: " (* (.indexOf indexes index) 5) "%"))))

(defn matching-process-with-info [quotes]
  (let [quotes (vec quotes)
        index-to-log (gen-index-to-log quotes)]
    (reduce
      (fn [v quote]
        (do
          (log-progress (.indexOf quotes quote) index-to-log)
          (add-quote-to-vec v quote)))
      []
      quotes)))

(defn matching-process [quotes]
  (reduce
    (fn [v quote]
      (add-quote-to-vec v quote))
    []
    quotes))
