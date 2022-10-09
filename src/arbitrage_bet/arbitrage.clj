(ns arbitrage-bet.arbitrage
  (:require [arbitrage-bet.file-importer :as fi]
            [arbitrage-bet.levenshtein :as l]
            [java-time.api :as jt]))

(defn gen-name [match]
  (str (:nameTeam1 match) " - " (:nameTeam2 match)))

(defn create-match-quotes [match]
  {:date (:date match)
   :name (gen-name match)
   :matches [match]})

(defn add-match [quotes match]
  (let [matches (:matches quotes)
        matches-u (conj matches match)]
    (conj quotes {:matches matches-u})))

(defn name-match-matching? [candidate match]
  (> 10 (l/levenshtein (:name candidate) (gen-name match))))

(defn date-time-matching? [candidate match]
  (< (jt/time-between (:date candidate)
                      (:date match) :minutes)
     15))

(defn match-matching? [candidate match]
  (and (name-match-matching? candidate match)
       (date-time-matching? candidate match)))

(defn replace-in [v quotes index]
  (vec (concat (subvec v 0 index)
               [quotes]
               (subvec v (inc index)))))

(defn replace-or-add
  ([vec match]
   (replace-or-add vec match 0))
  ([vec match index]
   (if (= (.size vec) index)
     (conj vec (create-match-quotes match))
     (if (match-matching? (.get vec index) match)
       (replace-in vec (add-match (.get vec index) match) index)
       (recur vec match (inc index))))))

(defn sort-by-match [data]
  (reduce
    replace-or-add
    []
    data))

(defn stat [data]
  (reduce
    (fn [result [k v]]
      (if (nil? v)
        (prn "Nil key")
        (if (nil? (get result (.size v)))
          (prn "size: " (.size v))
          (conj result {(.size v) (+ 1 (get result (.size v)))}))))
    {0 0 1 0 2 0 3 0 4 0 5 0}
    data))

(defn find-arbitrage []
  (let [result (-> (fi/load-data)
                   (sort-by-match))]
    (do
      (fi/export-result result)
      (stat result))))