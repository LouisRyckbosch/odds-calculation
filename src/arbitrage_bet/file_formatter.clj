(ns arbitrage-bet.file-formatter
  (:require [arbitrage-bet.date-formatter :as df]
            [java-time.api :as jt]))

(defn deaccent [str]
  (if (nil? str)
    ""
    (let [normalized (java.text.Normalizer/normalize str java.text.Normalizer$Form/NFD)]
      (clojure.string/replace normalized #"\p{InCombiningDiacriticalMarks}+" ""))))

(defn format-team-name [name]
  (-> (deaccent name)
      (.replaceAll "\\s" "")
      (.toUpperCase)
      (.replaceAll "CLUB" "")))

(defn get-date [match]
  (if-let [time (get match "time")]
    (str (get match "date") "#" time)
    (get match "date")))

(defn format-match [match site]
  {:match      (get match "match")
   :site       site
   :quoteTeam1 (get match "quoteTeam1")
   :quoteTeam2 (get match "quoteTeam2")
   :quoteDraw  (get match "quoteDraw")
   :nameTeam1  (format-team-name (get match "nameTeam1"))
   :nameTeam2  (format-team-name (get match "nameTeam2"))
   :date       (df/handle-dates (get-date match) site)})

(defn format-file [json]
  (let [site (get json "site")
        data (get json "data")]
    (reduce
      (fn [col match]
        (conj col (format-match match site))
        )
      []
      data)))
