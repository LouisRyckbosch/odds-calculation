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
      (.replaceAll "CLUB" "")
      (.replaceAll "SPOR" "")
      (.replaceAll "AS" "")
      (.replaceAll "MELBOURNE" "")
      (.replaceAll "FC" "")))

(defn get-date [match]
  (if-let [time (get match "time")]
    (str (get match "date") "#" time)
    (get match "date")))

(defn handle-date [match site]
  (try (df/handle-dates (get-date match) site)
       (catch Exception e (do (prn (str "Date Invalid on match : " (get match "match")
                                        " | Site : " (str site)))
                              :undefined))))

(defn parse-quote [quote match site]
  (try (Double/parseDouble (.replace quote "," "."))
       (catch Exception e (do (prn (str "Quote invalid on match : " (get match "match")
                                        " | Site : " (str site)))
                              nil))))

(defn format-match [match site]
  {:match      (get match "match")
   :site       site
   :quoteTeam1 (parse-quote (get match "quoteTeam1") match site)
   :quoteTeam2 (parse-quote (get match "quoteTeam2") match site)
   :quoteDraw  (parse-quote (get match "quoteDraw") match site)
   :nameTeam1  (format-team-name (get match "nameTeam1"))
   :nameTeam2  (format-team-name (get match "nameTeam2"))
   :date       (handle-date match site)})

(defn format-file [json]
  (let [site (get json "site")
        data (get json "data")]
    (reduce
      (fn [col match]
        (conj col (format-match match site))
        )
      []
      data)))
