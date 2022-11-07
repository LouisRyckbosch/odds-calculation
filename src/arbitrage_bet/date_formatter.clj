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
  (let [[date time] (str/split date-and-time #"#")
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
    "LUNDI" :monday
    "MARDI" :tuesday
    "MERCREDI" :wednesday
    "JEUDI" :thursday
    "VENDREDI" :friday
    "SAMEDI" :saturday
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

(defn parse-barriere [date]
  (jt/local-date-time))

(defn france-paris [date]
  (jt/local-date-time))

(defn parse-date-netbet [date]
  (cond
    (.contains date "Auj.") (jt/local-date)
    (.contains date "Dem.") (jt/plus (jt/local-date) (jt/days 1))
    :else (let [[day month] (map #(Long/valueOf %) (str/split date #"/"))
                year (jt/as (jt/local-date) :year)]
            (jt/local-date year month day))))

(defn parse-netbet [date]
  (let [[date time] (str/split date #"\n")
        date (parse-date-netbet date)
        [year month day] (jt/as date :year :month-of-year :day-of-month)
        [hh mm] (map #(Long/valueOf %) (str/split time #":"))]
    (jt/local-date-time year month day hh mm)))

(defn parse-france-date [date]
  (cond
    (.equals "AUJ" date) (jt/local-date)
    :else (let [[day month] (map #(Long/valueOf %) (str/split date #"/"))]
            (jt/local-date (jt/as (jt/local-date) :year)
                           month
                           day))))

(defn parse-france-time [time]
  (if (.contains time "'")
    [(jt/as (jt/local-date-time) :hour-of-day) 00]
    (map #(Long/valueOf %) (str/split time #":"))))

(defn parse-france [date]
  (let [[date time] (str/split date #" - ")
        date (parse-france-date date)
        [year month day] (jt/as date :year :month-of-year :day-of-month)
        [hh mm] (parse-france-time time)]
    (java-time.api/local-date-time year month day hh mm)))

(defn parse-pmu [date]
  (let [[date time] (str/split date #"#")
        [year month day] (map #(Long/valueOf %) (str/split date #"-"))
        [hh mm] [12 00]]
    (java-time.api/local-date-time year month day hh mm)))

(defn month-verbose-to-month-vbet [month]
  (let [months (->> (month-formatted)
                    (map #(subs % 0 3)))
        month (-> (subs month 0 3)
                  (.toUpperCase))]
    (+ 1 (.indexOf months month))))

(defn parse-vbet [date]
  (let [date (subs date 5)
        [day month-verbose year] (str/split date #" ")]
    (java-time.api/local-date-time (Long/valueOf year)
                                   (month-verbose-to-month-vbet month-verbose)
                                   (Long/valueOf day)
                                   12
                                   00)))

(defn parse-date-zebet [date]
  [(jt/as (jt/local-date) :year)
   (month-verbose-to-month-vbet date)
   (Long/valueOf (subs date 5 7))])

(defn convert-time-zebet [time]
  (let [[time ampm] (str/split time #" ")
        [hour min] (map #(Long/valueOf %) (str/split time #":"))]
    (if (.equals "am" ampm)
      [hour min]
      [(+ 12 hour) min])))

(defn parse-zebet [date]
  (let [time (subs date 8)
        date (subs date 0 8)
        [year month day] (parse-date-zebet date)
        [hour mm] (convert-time-zebet time)]
    (java-time.api/local-date-time year month day hour mm)))

(defn parse-pokerstars [date]
  (let [[date time] (str/split date #" -  ")
        date (second (str/split date #", "))
        [hh mm] (map #(Long/valueOf %) (str/split (subs time 0 5) #":"))]
    (java-time.api/local-date-time (jt/as (jt/local-date) :year)
                                   (month-verbose-to-month-vbet (subs date 2 5))
                                   (Long/valueOf (clojure.string/trim (subs date 0 2)))
                                   hh
                                   mm)))

(defn handle-dates [date site]
  (case site
    "BARRIEREBET" (parse-barriere date)
    "BETCLIC" (parse-betclic date)
    "BWIN" (parse-bwin date)
    "FRANCEPARIS" (parse-france date)
    "NETBET" (parse-netbet date)
    "PARIONSSPORT" (parse-ps date)
    "PMU" (parse-pmu date)
    "POKERSTARS" (parse-pokerstars date)
    "UNIBET" (parse-unibet date)
    "VBET" (parse-vbet date)
    "WINAMAX" (parse-winamax date)
    "ZEBET" (parse-zebet date)))