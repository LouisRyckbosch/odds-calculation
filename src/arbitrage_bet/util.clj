(ns arbitrage-bet.util
  (:require [arbitrage-bet.computer :as c]
            [clojure.string :as str]))

(defn fractional-to-decimal [string]
  (let [[num deno] (str/split string #"/")]
    (+ 1 (/ (Long/parseLong num) (Long/parseLong deno)))))

(defn parse [string]
  (let [odds-frac (str/split string #";")
        quotes (map fractional-to-decimal odds-frac)]
    (c/calc-implied-probability quotes)))