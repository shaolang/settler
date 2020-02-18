(ns settler.core-test
  (:require [clojure.string :as str]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :refer [for-all]]
            [settler.core :as settler])
  (:import [java.time DayOfWeek LocalDate]))

;;;;;;;;;;;;;
;; generators

(def gen-currency (gen/fmap #(str/upper-case (apply str %))
                            (gen/vector gen/char-alphanumeric 3)))


(def gen-date (gen/fmap #(.plusDays (LocalDate/now) %) gen/small-integer))


(def gen-config (gen/fmap #(hash-map :weekends (apply hash-set %))
                          (gen/set (gen/elements (DayOfWeek/values))
                                   {:min-elements 2 :max-elements 3})))

;;;;;;;;;;;;;
;; properties

(defspec nothing-falls-on-weekends
  (for-all [configs     (gen/map gen-currency gen-config {:num-elements 2})
            spot-lag    (gen/choose 0 3)
            trade-date  gen-date]
    (let [all-weekends  (into #{} (mapcat :weekends (vals configs)))
          pair          (into #{} (keys configs))
          [ccy1 ccy2]   (keys configs)
          spot-lag-map  {pair spot-lag}
          spot-date     (settler/spot spot-lag-map configs trade-date ccy1 ccy2)
          spot-day      (.getDayOfWeek spot-date)]
      (not (some #{spot-day} all-weekends)))))
