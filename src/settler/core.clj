(ns settler.core
  (:require [clojure.set :as set])
  (:import [java.time DayOfWeek]
           [java.time.temporal ChronoUnit TemporalAdjusters]))

(defonce ^:private STANDARD-WEEKEND #{DayOfWeek/SATURDAY DayOfWeek/SUNDAY})
(defonce ^:private STANDARD-SPOT-LAG 2)
(defonce ^:private UNITS {"W" ChronoUnit/WEEKS
                          "M" ChronoUnit/MONTHS
                          "Y" ChronoUnit/YEARS})


(defn- latest-date [[d1 d2]]
  (if (.isAfter d1 d2) d1 d2))


(defn- weekend? [date weekends]
  (some #{(.getDayOfWeek date)} weekends))


(defn- holiday? [date holidays]
  (some #{date} holidays))


(defn- next-biz-day
  ([date days-to-add config]
   (next-biz-day date days-to-add nil nil config))

  ([date days-to-add currency usd-config {:keys [weekends holidays]}]
   (let [usd-holidays  (:holidays usd-config)
         non-usd?      (not= currency "USD")
         weekends      (if weekends weekends STANDARD-WEEKEND)]
     (loop [date        date
            days-to-add days-to-add]
       (let [[date days-to-add]  (if (pos? days-to-add)
                                   [(.plusDays date 1) (dec days-to-add)]
                                   [date days-to-add])]
         (cond
          (or (weekend? date weekends)
              (and non-usd? (holiday? date holidays))
              (and (zero? days-to-add) (holiday? date usd-holidays)))
          (recur date (inc days-to-add))

          (pos? days-to-add)
          (recur date days-to-add)

          :else
          date))))))


(defn- last-biz-day-of-month? [date config]
  (not= (.getMonth date)
        (.getMonth (next-biz-day date 1 config))))


(defn- prev-biz-day [date {:keys [weekends holidays]}]
  (loop [date date]
    (let [candidate (.minusDays date 1)]
      (if (or (weekend? candidate weekends) (holiday? candidate holidays))
        (recur candidate)
        candidate))))


(defn tenor [x]
  (let [[_ n unit] (re-matches #"^(\d+)(\w)$" x)]
    {:n     (Long/parseLong n)
     :unit  (get UNITS unit)}))


(defn spot [spot-lags configs trade-date ccy1 ccy2]
  (let [pair        #{ccy1 ccy2}
        spot-lag    (get spot-lags pair STANDARD-SPOT-LAG)
        usd-config  (get configs "USD")
        spots       (map #(next-biz-day trade-date spot-lag % usd-config
                                        (get configs %))
                         pair)
        ccy-configs (map #(get configs %) pair)
        candidate   (latest-date spots)]
    (next-biz-day candidate
                  0
                  nil
                  usd-config
                  {:weekends (apply set/union (map :weekends ccy-configs))
                   :holidays (apply set/union (map :holidays ccy-configs))})))


(defn value-date [spot-lags configs trade-date tenor-str ccy1 ccy2]
  (let [weekends          (apply set/union (map #(get-in configs [% :weekends])
                                                [ccy1 ccy2 "USD"]))
        weekends          (if weekends weekends STANDARD-WEEKEND)
        holidays          (apply set/union (map #(get-in configs [% :holidays])
                                                [ccy1 ccy2 "USD"]))
        ccy-configs       {:weekends weekends :holidays holidays}
        spot-date         (spot spot-lags configs trade-date ccy1 ccy2)
        {:keys [n unit]}  (tenor tenor-str)
        candidate         (.plus spot-date n unit)]
    (if (or (= unit ChronoUnit/WEEKS)
            (not (last-biz-day-of-month? spot-date ccy-configs)))
      (next-biz-day candidate 0 ccy-configs)
      (let [candidate (.with candidate (TemporalAdjusters/lastDayOfMonth))]
        (if (or (weekend? candidate weekends) (holiday? candidate holidays))
          (prev-biz-day candidate ccy-configs)
          candidate)))))
