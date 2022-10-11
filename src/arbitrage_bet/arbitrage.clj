(ns arbitrage-bet.arbitrage
  (:require [arbitrage-bet.file-importer :as fi]
            [arbitrage-bet.levenshtein :as l]
            [java-time.api :as jt]
            [arbitrage-bet.computer :as c]
            [arbitrage-bet.matching-process :as mp]))

(defn find-arbitrage []
  (-> (fi/load-data)
      (mp/matching-process)
      (c/compute-quotes)
      (fi/export-result)))

(defn test-computing []
  (-> (fi/import-result)
      (c/compute-quotes)))