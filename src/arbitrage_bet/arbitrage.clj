(ns arbitrage-bet.arbitrage
  (:require [arbitrage-bet.file-importer :as fi]
            [arbitrage-bet.levenshtein :as l]
            [java-time.api :as jt]
            [arbitrage-bet.computer :as c]))

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
  (< (Math/abs (jt/time-between (:date candidate)
                                (:date match) :minutes))
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

(defn find-arbitrage []
  (-> (fi/load-data)
      (sort-by-match)
      (c/compute-quotes)
      (fi/export-result)))

(defn test-computing []
  (-> (fi/import-result)
      (c/compute-quotes)))