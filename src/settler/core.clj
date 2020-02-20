(ns settler.core
  (:require [clojure.set :as set])
  (:import [java.time DayOfWeek]))

(defonce ^:private STANDARD-WEEKEND #{DayOfWeek/SATURDAY DayOfWeek/SUNDAY})
(defonce ^:private STANDARD-SPOT-LAG 2)


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
         (and is-ccy-holiday (not is-usd-holiday))
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
