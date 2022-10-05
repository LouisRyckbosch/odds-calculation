(ns arbitrage-bet.arbitrage
  (:require [arbitrage-bet.file-importer :as fi]))

(defn match-key [match]
  (str (:nameTeam1 match) " - " (:nameTeam2 match)))

(defn add-match [map key match]
  (let [matchs (get map key)
        matchs-u (conj matchs match)]
    (conj map {key matchs-u})))

(defn sort-by-match [data]
  (reduce
    (fn [map-by-match match]
      (let [key (match-key match)]
        (if (empty? (get map-by-match key))
          (conj map-by-match {key [match]})
          (add-match map-by-match key match))))
    {}
    data))

(defn stat [data]
  (reduce
    (fn [result [k v]]
      (conj result {(.size v) (+ 1 (get result (.size v)))}))
    {0 0 1 0 2 0 3 0 4 0 5 0}
    data))

(defn find-arbitrage []
  (-> (fi/load-data)
      (sort-by-match)
      (stat)))
