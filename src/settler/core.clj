(ns settler.core
  (:require [clojure.set :as set])
  (:import [java.time DayOfWeek]
           [java.time.temporal ChronoUnit]))

(defonce ^:private STANDARD-WEEKEND #{DayOfWeek/SATURDAY DayOfWeek/SUNDAY})
(defonce ^:private STANDARD-SPOT-LAG 2)
(defonce ^:private UNITS {"W" ChronoUnit/WEEKS
                          "M" ChronoUnit/MONTHS
                          "Y" ChronoUnit/YEARS})


(defn- latest-date [[d1 d2]]
  (if (.isAfter d1 d2) d1 d2))


(defn- next-biz-day [currency date days-to-add usd-config
                     {:keys [weekends holidays] :as config}]
  (let [[date days-to-add]  (if (pos? days-to-add)
                              [(.plusDays date 1) (dec days-to-add)]
                              [date days-to-add])
        weekends            (if weekends weekends STANDARD-WEEKEND)
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


(defn tenor [x]
  (let [[_ n unit] (re-matches #"^(\d+)(\w)$" x)]
    {:n (Long/parseLong n)
     :unit (get UNITS unit)}))


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


(defn value-date [tenor-str spot-lags configs trade-date ccy1 ccy2]
  (let [{:keys [n unit]}  (tenor tenor-str)
        spot-date         (spot spot-lags configs trade-date ccy1 ccy2)
        ccy-configs       (map #(get configs %) [ccy1 ccy2])
        candidate         (.plus spot-date n unit)]
    (next-biz-day nil candidate 0 (get configs "USD")
                  {:weekends (apply set/union (map :weekends ccy-configs))
                   :holidays (apply set/union (map :holidays ccy-configs))})))
