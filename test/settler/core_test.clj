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


(def gen-holidays (gen/set (gen/fmap #(.plusDays (LocalDate/now) %)
                                     (gen/choose -10 10))
                           {:min-elements 1 :max-elements 20}))


(def gen-config (gen/fmap (fn [[weekends holidays]]
                            (hash-map :weekends (apply hash-set weekends)
                                      :holidays holidays))
                          (gen/tuple (gen/set (gen/elements (DayOfWeek/values))
                                              {:min-elements 2 :max-elements 3})
                                     gen-holidays)))

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


(defspec nothing-falls-on-holidays
  (for-all [configs     (gen/map gen-currency gen-config {:num-elements 2})
            spot-lag    (gen/choose 0 3)
            trade-date  gen-date]
    (let [all-holidays  (into #{} (mapcat :holidays (vals configs)))
          [ccy1 ccy2]   (keys configs)
          spot-date     (settler/spot nil configs trade-date ccy1 ccy2)]
      (and (not (some #{spot-date} all-holidays))
           (seq all-holidays)))))


(defspec nothing-falls-on-usd-holidays-even-for-crosses
  (for-all [ccys          (gen/set gen-currency {:num-elements 2})
            spot-lag      (gen/choose 0 3)
            trade-date    gen-date
            usd-holidays  gen-holidays]
    (let [[ccy1 ccy2]   (vec ccys)
          spot-lags     {ccys spot-lag}
          configs       (->  ccys
                            (zipmap (repeat nil))
                            (assoc "USD" {:holidays usd-holidays}))
          spot-date     (settler/spot spot-lags configs trade-date ccy1 ccy2)]
      (not (some #{spot-date} usd-holidays)))))

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
  (let [trade-date  (LocalDate/of 2015 1 1)         ;; Thursday
        spot-date   (settler/spot nil nil trade-date "GHI" "JKL")
        spot-day    (.getDayOfWeek spot-date)]
    (is (= spot-date (LocalDate/of 2015 1 5)))))    ;; Monday


(deftest USD-holidays-on-T+1-considered-good
  (let [trade-date  (LocalDate/of 2023 7 3)         ;; Monday
        config      {"USD" {:holidays #{(LocalDate/of 2023 7 4)}}}
        spot-lags   {#{"USD" "CAD"} 1}
        spot-fn     (partial settler/spot spot-lags config trade-date)]
    (testing "spot-lag is 2"
      (is (= (LocalDate/of 2023 7 5)                ;; Wednesday
             (spot-fn "USD" "JPY"))))

    (testing "spot-lag is 1"
      (is (= (LocalDate/of 2023 7 5)
             (spot-fn "USD" "CAD"))))))


(deftest non-USD-holidays-on-T+1-considered-not-good-biz-day
  (let [trade-date  (LocalDate/of 2019 4 30)        ;; Tuesday
        spot-date   (settler/spot nil
                                  {"ABC" {:holidays #{(LocalDate/of 2019 5 1)}}
                                   "XYZ" nil}
                                  trade-date
                                  "ABC"
                                  "XYZ")]
    (is (= spot-date (LocalDate/of 2019 5 3)))))    ;; Friday


(deftest USD-and-non-USD-holidays-on-T+1-considered-not-good-biz-day
  (let [trade-date  (LocalDate/of 2023 7 3)         ;; Monday
        holiday     (LocalDate/of 2023 7 4)
        config      {"USD" {:holidays #{holiday}}
                     "SGD" {:holidays #{holiday}}}
        spot-date   (settler/spot nil config trade-date "USD" "SGD")]
    (is (= (LocalDate/of 2023 7 6)
           spot-date))))
