(ns arbitrage-bet.matching-process
  (:require [arbitrage-bet.levenshtein :as l]))

(defn not-excluded? [leven candidate]
  (>= (.length (:name candidate)) leven))

(defn gen-name [quote]
  (str (:nameTeam1 quote) " - " (:nameTeam2 quote)))

(defn add-leven [quote leven]
  (conj quote {:leven-score leven}))

(defn create-match [quote]
  {:date   (:date quote)
   :name   (gen-name quote)
   :quotes [(add-leven quote 0)]})

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
  (let [quotes (filter #(= (:site quote) (:site %)) (:quotes match))
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
    (if (not-excluded? leven match)
      (update-or-replace-if-best v match quote leven)
      (conj-in-v-struct v match))))

(defn add-quote-to-vec [v quote]
  (let [v-quote-added (reduce (fn [v-up match]
                                (add-if-best-match v-up match quote))
                              {:data  []
                               :added? false}
                              v)]
    (if (not (:added? v-quote-added))
      (create-and-add (:data v-quote-added) quote)
      (:data v-quote-added))))

(defn matching-process [quotes]
  (reduce
    (fn [v quote]
      (add-quote-to-vec v quote))
    []
    quotes))
