(ns settler.core-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :refer [for-all]]
            [settler.core :as settler])
  (:import [java.time DayOfWeek LocalDate]
           [java.time.temporal ChronoUnit]))

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

;;;;;;;;;;
;; helpers

(defn days-between [start end]
  (.until start end ChronoUnit/DAYS))


(defn rand-date []
  (.plusDays (LocalDate/now) (rand-int 100)))

;;;;;;;;
;; tests

(deftest spot-lag-determines-days-between-trade-and-spot-date
  (doseq [spot-lag [0 1 2 3]]
    (testing (str "spot-lag = " spot-lag)
      (let [configs       (zipmap ["ABC" "DEF"] (repeat {:weekends #{}}))
            spot-lag-map  {#{"ABC" "DEF"} spot-lag}
            trade-date    (rand-date)
            spot-date     (settler/spot spot-lag-map configs trade-date
                                        "ABC" "DEF")]
        (is (= spot-lag (days-between trade-date spot-date)))))))


(deftest spot-lag-defaults-to-2-if-not-setup
  (let [configs     (zipmap ["UVW" "XYZ"] (repeat {:weekends #{}}))
        trade-date  (rand-date)
        spot-date   (settler/spot {} configs trade-date "UVW" "XYZ")]
    (is (= 2 (days-between trade-date spot-date)))))


(deftest weekends-defaults-to-sats-and-suns-if-not-setup
  (let [trade-date  (LocalDate/of 2015 1 1)   ;; Thursday
        spot-date   (settler/spot nil nil trade-date "GHI" "JKL")
        spot-day    (.getDayOfWeek spot-date)]
    (is (= spot-date (LocalDate/of 2015 1 5)))))    ;; Tuesday
