(ns arbitrage-bet.file-formatter
  (:require [java-time.api :as jt]))

(def now (jt/local-date))

(defn deaccent [str]
  (if (nil? str)
    ""
    (let [normalized (java.text.Normalizer/normalize str java.text.Normalizer$Form/NFD)]
      (clojure.string/replace normalized #"\p{InCombiningDiacriticalMarks}+" ""))))

(defn format-date [date]
  (jt/format "dd/MM/yyyy" (jt/zoned-date-time date)))

(defn parse-date [date]
  (jt/local-date "MM/yyyy/dd" (.substring date 0 10)))

(defn get-now []
  (format-date arbitrage-bet.file-formatter/now))

(defn get-tomorrow []
  (format-date (jt/plus arbitrage-bet.file-formatter/now (jt/days 1))))

(defn contains-today-keyword [date]
  (-> (deaccent date)
      (.toUpperCase)
      (.replaceAll "'" "")
      (.replaceAll "\\s" "")
      (.contains "AUJOURDHUI")))

(defn contains-tomorrow-keyword [date]
  (-> (deaccent date)
      (.toUpperCase)
      (.replaceAll "'" "")
      (.replaceAll "\\s" "")
      (.contains "DEMAIN")))

(defn parse [])

(defn formattable-as-in [site]
  (contains? ["BWIN", "BETCLIC"] site))

(defn handle-dates [date site]
  (cond
    ;;(contains-today-keyword date) (get-now)
    ;; (contains-tomorrow-keyword date) (get-tomorrow)
    ;;(formattable-as-in site)
    ;;
    ;;(-> (parse-date date)
    ;;(format-date)
    )
    true true)

(defn format-team-name [name]
  (-> (deaccent name)
      (.replaceAll "\\s" "")
      (.toUpperCase )))

(defn format-match [match site]
  {:match      (get match "match")
   :site       site
   :quoteTeam1 (get match "quoteTeam1")
   :quoteTeam2 (get match "quoteTeam2")
   :quoteDraw  (get match "quoteDraw")
   :nameTeam1  (format-team-name (get match "nameTeam1"))
   :nameTeam2  (format-team-name (get match "nameTeam2"))
   :date       (handle-dates (get match "date") site)})

(defn format-file [json]
  (let [site (get json "site")
        data (get json "data")]
    (reduce
      (fn [col match]
        (conj col (format-match match site))
        )
      []
      data)))
