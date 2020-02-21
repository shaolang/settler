(ns settler.core
  (:require [clojure.set :as set])
  (:import [java.time DayOfWeek]
           [java.time.temporal ChronoField TemporalField]))

(defonce ^:private STANDARD-WEEKEND #{DayOfWeek/SATURDAY DayOfWeek/SUNDAY})
(defonce ^:private STANDARD-SPOT-LAG 2)
(defonce ^:private TENORS
  {"1W"   {:n  1 :unit ChronoField/ALIGNED_WEEK_OF_YEAR}
   "2W"   {:n  2 :unit ChronoField/ALIGNED_WEEK_OF_YEAR}
   "3W"   {:n  3 :unit ChronoField/ALIGNED_WEEK_OF_YEAR}
   "1M"   {:n  1 :unit ChronoField/MONTH_OF_YEAR}
   "2M"   {:n  2 :unit ChronoField/MONTH_OF_YEAR}
   "3M"   {:n  3 :unit ChronoField/MONTH_OF_YEAR}
   "4M"   {:n  4 :unit ChronoField/MONTH_OF_YEAR}
   "5M"   {:n  5 :unit ChronoField/MONTH_OF_YEAR}
   "6M"   {:n  6 :unit ChronoField/MONTH_OF_YEAR}
   "7M"   {:n  7 :unit ChronoField/MONTH_OF_YEAR}
   "8M"   {:n  8 :unit ChronoField/MONTH_OF_YEAR}
   "9M"   {:n  9 :unit ChronoField/MONTH_OF_YEAR}
   "10M"  {:n 10 :unit ChronoField/MONTH_OF_YEAR}
   "11M"  {:n 11 :unit ChronoField/MONTH_OF_YEAR}
   "1Y"   {:n  1 :unit ChronoField/YEAR}
   "2Y"   {:n  2 :unit ChronoField/YEAR}})


(defn- latest-date [[d1 d2]]
  (if (.isAfter d1 d2) d1 d2))


(defn- next-biz-day [currency date days-to-add usd-config
                     {:keys [weekends holidays] :as config
                      :or {weekends STANDARD-WEEKEND}}]
  (let [[date days-to-add]  (if (pos? days-to-add)
                              [(.plusDays date 1) (dec days-to-add)]
                              [date days-to-add])
        is-weekend          (some #{(.getDayOfWeek date)} weekends)
        is-ccy-holiday      (some #{date} holidays)
        is-usd-holiday      (some #{date} (:holidays usd-config))]
    (cond
     (or is-weekend
         (and is-ccy-holiday (not= currency "USD"))
         (and is-usd-holiday (zero? days-to-add)))
     (recur currency date (inc days-to-add) usd-config config)

     (pos? days-to-add)
     (recur currency date days-to-add usd-config config)

     :else
     date)))


(defn spot [spot-lags configs trade-date ccy1 ccy2]
  (let [pair        #{ccy1 ccy2}
        spot-lag    (get spot-lags pair STANDARD-SPOT-LAG)
        usd-config  (get configs "USD")
        spots       (map #(next-biz-day % trade-date spot-lag usd-config
                                        (get configs %))
                         pair)
        ccy-configs (map #(get configs %) pair)
        candidate   (latest-date spots)]
    (next-biz-day nil
                  candidate
                  0
                  usd-config
                  {:weekends (apply set/union (map :weekends ccy-configs))
                   :holidays (apply set/union (map :holidays ccy-configs))})))


(defn value-date [tenor spot-lags configs trade-date ccy1 ccy2]
  (let [{:keys [n unit]}  (get TENORS tenor)
        spot-date         (spot spot-lags configs trade-date ccy1 ccy2)
        ccy-configs       (map #(get configs %) [ccy1 ccy2])
        candidate         (.with spot-date unit (long n))]
    (next-biz-day nil candidate 0 (get configs "USD")
                  {:weekends (apply set/union (map :weekends ccy-configs))
                   :holidays (apply set/union (map :holidays ccy-configs))})))
