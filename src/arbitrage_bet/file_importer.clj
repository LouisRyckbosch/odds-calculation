(ns arbitrage-bet.file-importer
  (:require [clojure.data.json :as json]
            [arbitrage-bet.file-formatter :as f]))

(defn get-repertory []
  "C:\\Users\\Louis\\Documents\\my code\\scrapper-odds\\json\\")

(defn get-name-export []
  "C:\\Users\\Louis\\Documents\\result.json")

(defn get-list-of-files []
  (seq (.list (clojure.java.io/file (get-repertory)))))

(defn get-full-name [filename]
  (str (get-repertory) filename))

(defn read-file [filename]
  (json/read-str (slurp (get-full-name filename))))

(defn date-value-writer [key value]
  (if (= key :date)
    (.toString value)
    value))

(defn export-result [data]
  (spit (get-name-export) (json/write-str data :value-fn date-value-writer)))

(defn import-result []
  (json/read-str (slurp (get-full-name "export.json"))))

(defn load-data []
  (reduce
    (fn [datas file]
      (->> (read-file file)
           (f/format-file)
           (into datas)))
    []
    (get-list-of-files)))