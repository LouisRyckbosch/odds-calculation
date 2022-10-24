(ns arbitrage-bet.date-formatter
  (:require [java-time.api :as jt]
            [clojure.string :as str]))

(defn parse-date-betclic-bwin [date]
  (cond
    (.contains date "Aujourd'hui") (jt/local-date)
    (.contains date "Commence dans") (jt/local-date)
    (.contains date "Demain") (jt/plus (jt/local-date) (jt/days 1))
    (.contains date "Après-demain") (jt/plus (jt/local-date) (jt/days 2))
    :else (jt/local-date "dd/MM/yyyy" date)))

(defn parse-betclic [date-and-time]
  (let [[date time] (str/split date-and-time #" ")
        date (parse-date-betclic-bwin date)
        [year month day] (jt/as date :year :month-of-year :day-of-month)
        [hh mm] (map #(Long/valueOf %) (str/split time #":"))]
    (java-time.api/local-date-time year month day hh mm)))

(defn split-date-and-time-bwin [date]
  (if (.contains date "Commence dans")
    (let [split (str/split date #" ")]
      [(str (.get split 0) " " (.get split 1)) (.get split 2)])
    (if (= (count (str/split date #"/")) 3)
      (str/split date #" ")
      (str/split date #"/"))))

(defn parse-hours-mins-bwin [time]
  (if (.contains time ":")
    (map #(Long/valueOf %) (str/split time #":"))
    [0 1]))

(defn parse-bwin [date-and-time]
  (let [[date time] (split-date-and-time-bwin date-and-time)
        date (parse-date-betclic-bwin date)
        [year month day] (jt/as date :year :month-of-year :day-of-month)
        [hh mm] (parse-hours-mins-bwin time)]
    (java-time.api/local-date-time year month day hh mm)))

(defn split-date-time-ps-unibet [date]
  (str/split date #"#"))

(defn month []
  ["Janvier" "février" "mars" "avril" "Mai" "Juin" "Juillet" "Août" "Septembre" "Octobre" "Novembre" "Décembre"])

(defn deaccent [str]
  (if (nil? str)
    ""
    (let [normalized (java.text.Normalizer/normalize str java.text.Normalizer$Form/NFD)]
      (clojure.string/replace normalized #"\p{InCombiningDiacriticalMarks}+" ""))))

(defn month-formatted []
  (map
    #(-> (deaccent %)
         (.toUpperCase))
    (month)))

(defn month-verbose-to-month [month]
  (+ 1 (.indexOf (month-formatted) (-> (deaccent month)
                                       (.toUpperCase)))))

(defn parse-date-ps-unibet [date]
  (let [[no-use day month-verbose] (str/split date #" ")
        month (month-verbose-to-month month-verbose)
        year (jt/as (jt/local-date) :year)]
    (jt/local-date year month (Long/valueOf day))))

(defn parse-ps [date-and-time]
  (let [[date time] (split-date-time-ps-unibet date-and-time)
       date (parse-date-ps-unibet date)
       [year month day] (jt/as date :year :month-of-year :day-of-month)
       [hh mm] (map #(Long/valueOf %) (str/split (str/trim time) #"h"))]
   (java-time.api/local-date-time year month day hh mm)))

(defn translate-day [day]
  (case day
    "LUNDI"    :monday
    "MARDI"    :tuesday
    "MERCREDI" :wednesday
    "JEUDI"    :thursday
    "VENDREDI" :friday
    "SAMEDI"   :saturday
    "DIMANCHE" :sunday))

(defn parse-day-winamax [day]
  (let [keyword-day (translate-day day)]
    (jt/adjust (jt/local-date) :next-day-of-week keyword-day)))

(defn parse-winamax-date [date]
  (cond
    (.contains date "AUJOURD’HUI") (jt/local-date)
    (.contains date "DEMAIN") (jt/plus (jt/local-date) (jt/days 1))
    :else (parse-day-winamax date)))

(defn parse-winamax [date]
  (let [[date time] (str/split date #" À ")
        date (parse-winamax-date date)
        [year month day] (jt/as date :year :month-of-year :day-of-month)
        [hh mm] (map #(Long/valueOf %) (str/split time #":"))]
    (java-time.api/local-date-time year month day hh mm)))

(defn parse-unibet [date]
  (let [[date time] (split-date-time-ps-unibet date)
        date (parse-date-ps-unibet date)
        [year month day] (jt/as date :year :month-of-year :day-of-month)
        [hh mm] (map #(Long/valueOf %) (str/split time #":"))]
    (java-time.api/local-date-time year month day hh mm)))

(defn handle-dates [date site]
  (case site
    "BETCLIC" (parse-betclic date)
    "BWIN" (parse-bwin date)
    "PARIONSSPORT" (parse-ps date)
    "WINAMAX" (parse-winamax date)
    "UNIBET" (parse-unibet date)
    "BARRIEREBET"))