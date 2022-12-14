(ns arbitrage-bet.arbitrage
  (:require [arbitrage-bet.file-importer :as fi]
            [arbitrage-bet.computer :as c]
            [arbitrage-bet.matching-process :as mp]
            [arbitrage-bet.preprocessing :as p]
            [arbitrage-bet.stats :as stats]
            [arbitrage-bet.new-matching-process :as nmp]))

(defn find-arbitrage []
  (-> (fi/load-data)
      (p/filter-quote-no-data)
      (nmp/matching-process-with-info)
      (stats/get-stat-after-matching)
      (c/compute-quotes)
      (fi/export-result)))

(defn test-computing []
  (-> (fi/import-result)
      (c/compute-quotes)))