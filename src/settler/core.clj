(ns settler.core
  (:require [clojure.set :as set]))

(defn to-next-biz-day [weekends date days-to-add]
  (let [new-date (.plusDays date days-to-add)]
    (if (some #{(.getDayOfWeek new-date)} weekends)
      (recur weekends new-date 1)
      new-date)))


(defn spot [spot-lags configs trade-date ccy1 ccy2]
  (let [pair          #{ccy1 ccy2}
        spot-lag      (get spot-lags pair)
        ccy1-weekends (get-in configs [ccy1 :weekends])
        ccy2-weekends (get-in configs [ccy2 :weekends])
        spot1         (to-next-biz-day ccy1-weekends trade-date spot-lag)
        spot2         (to-next-biz-day ccy2-weekends trade-date spot-lag)
        candidate     (if (.isAfter spot1 spot2) spot1 spot2)]
    (to-next-biz-day (set/union ccy1-weekends ccy2-weekends) candidate 0)))
