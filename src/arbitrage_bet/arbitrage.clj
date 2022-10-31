(ns arbitrage-bet.arbitrage
  (:require [arbitrage-bet.file-importer :as fi]
            [arbitrage-bet.computer :as c]
            [arbitrage-bet.matching-process :as mp]
            [arbitrage-bet.preprocessing :as p]))

(defn find-arbitrage []
  (-> (fi/load-data)
      (p/filter-quote-no-data)
      (mp/matching-process-with-info)
      (c/compute-quotes)
      (fi/export-result)))

(defn test-computing []
  (-> (fi/import-result)
      (c/compute-quotes)))