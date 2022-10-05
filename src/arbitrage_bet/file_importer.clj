(ns arbitrage-bet.file-importer
  (:require [clojure.data.json :as json]
            [arbitrage-bet.file-formatter :as f]))

(defn get-list-of-files []
  ["betclic.json", "bwin.json", "parionssport.json", "unibet.json", "winamax.json"])

(defn get-repertory []
  "C:\\Users\\GLL\\Documents\\scrapper node js\\json\\")

(defn get-full-name [filename]
  (str (get-repertory) filename))

(defn read-file [filename]
  (json/read-str (slurp (get-full-name filename))))

(defn load-data []
  (reduce
    (fn [datas file]
      (->> (read-file file)
           (f/format-file)
           (into datas)))
    []
    (get-list-of-files)))