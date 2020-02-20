(ns settler.core
  (:require [clojure.set :as set])
  (:import [java.time DayOfWeek]))

(defonce ^:private STANDARD-WEEKEND #{DayOfWeek/SATURDAY DayOfWeek/SUNDAY})
(defonce ^:private STANDARD-SPOT-LAG 2)


(defn- max [[d1 d2]]
  (if (.isAfter d1 d2) d1 d2))


(defn- next-biz-day [currency date days-to-add
                     {:keys [weekends holidays] :as config
                      :or {weekends STANDARD-WEEKEND}}]
  (let [[date days-to-add]  (if (pos? days-to-add)
                              [(.plusDays date 1) (dec days-to-add)]
                              [date days-to-add])
        is-weekend          (some #{(.getDayOfWeek date)} weekends)
        is-holiday          (some #{date} holidays)]
    (cond
     (and (not is-weekend) is-holiday (= currency "USD") (= days-to-add 1))
     (recur currency date days-to-add config)

     (or is-weekend is-holiday)
     (recur currency date (inc days-to-add) config)

     (pos? days-to-add)
     (recur currency date days-to-add config)

     :else
     date)))


(defn spot [spot-lags configs trade-date ccy1 ccy2]
  (let [pair        #{ccy1 ccy2}
        spot-lag    (get spot-lags pair STANDARD-SPOT-LAG)
        spots       (map #(next-biz-day % trade-date spot-lag (get configs %))
                         pair)
        ccy-configs (map #(get configs %) pair)
        candidate   (max spots)]
    (next-biz-day nil
                  candidate
                  0
                  {:weekends (apply set/union (map :weekends ccy-configs))
                   :holidays (apply set/union (map :holidays ccy-configs))})))
