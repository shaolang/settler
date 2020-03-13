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


(defn- weekend? [date weekends]
  (some #{(.getDayOfWeek date)} weekends))


(defn- holiday? [date holidays]
  (some #{date} holidays))


(defn- next-biz-day [currency date days-to-add usd-config
                     {:keys [weekends holidays] :as config}]
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
         date)))))


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
        ccy-configs       {:weekends (apply set/union (map :weekends ccy-configs))
                           :holidays (apply set/union (map :holidays ccy-configs))}
        candidate         (.plus spot-date n unit)]
    (next-biz-day nil candidate 0 (get configs "USD") ccy-configs)))
