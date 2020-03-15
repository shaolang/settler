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


(defn gen-holidays
  ([] (gen-holidays nil))
  ([{:keys [start-date] :or {start-date (LocalDate/now)}}]
   (gen/set (gen/fmap #(.plusDays start-date %) (gen/choose -10 10))
            {:min-elements 1 :max-elements 20})))


(def ^:private gen-weekends (gen/set (gen/elements (DayOfWeek/values))
                                     {:min-elements 2 :max-elements 3}))

(defn gen-config
  ([] (gen-config nil))
  ([holiday-opts]
   (gen/fmap (fn [[weekends holidays]]
               (hash-map :weekends (apply hash-set weekends)
                         :holidays holidays))
             (gen/tuple gen-weekends
                        (gen-holidays holiday-opts)))))


(def gen-tenor (gen/let [tenor-unit     (gen/elements ["W" "M" "Y"])
                         tenor-duration (case tenor-unit
                                          "W"  (gen/choose 1 3)
                                          "M"  (gen/choose 1 11)
                                          (gen/choose 1 2))]
                 (apply str tenor-duration tenor-unit)))

;;;;;;;;;;;;;
;; properties

(defspec spot-date-never-falls-on-weekends
  (for-all [configs     (gen/map gen-currency (gen-config) {:num-elements 2})
            spot-lag    (gen/choose 0 3)
            trade-date  gen-date]
    (let [all-weekends  (into #{} (mapcat :weekends (vals configs)))
          pair          (into #{} (keys configs))
          [ccy1 ccy2]   (keys configs)
          spot-lag-map  {pair spot-lag}
          spot-date     (settler/spot spot-lag-map configs trade-date ccy1 ccy2)
          spot-day      (.getDayOfWeek spot-date)]
      (not (some #{spot-day} all-weekends)))))


(defspec spot-dates-never-falls-on-holidays
  (for-all [configs     (gen/map gen-currency (gen-config) {:num-elements 2})
            spot-lag    (gen/choose 0 3)
            trade-date  gen-date]
    (let [all-holidays  (into #{} (mapcat :holidays (vals configs)))
          [ccy1 ccy2]   (keys configs)
          spot-date     (settler/spot nil configs trade-date ccy1 ccy2)]
      (and (not (some #{spot-date} all-holidays))
           (seq all-holidays)))))


(defspec spot-date-never-falls-on-usd-holidays-even-for-crosses
  (for-all [ccys          (gen/set gen-currency {:num-elements 2})
            spot-lag      (gen/choose 0 3)
            trade-date    gen-date
            usd-holidays  (gen-holidays)]
    (let [[ccy1 ccy2]   (vec ccys)
          spot-lags     {ccys spot-lag}
          configs       (->  ccys
                            (zipmap (repeat nil))
                            (assoc "USD" {:holidays usd-holidays}))
          spot-date     (settler/spot spot-lags configs trade-date ccy1 ccy2)]
      (not (some #{spot-date} usd-holidays)))))


(defspec forward-value-dates-never-falls-on-weekends
  (for-all [tenor       gen-tenor
            trade-date  gen-date
            configs     (gen/map gen-currency (gen-config) {:num-elements 2})]
    (let [[ccy1 ccy2]   (keys configs)
          all-weekends  (into #{} (mapcat :weekends (vals configs)))
          vdate         (settler/value-date nil configs trade-date tenor
                                            ccy1 ccy2)
          vday        (.getDayOfWeek vdate)]
      (not (some #{vday} all-weekends)))))


(defspec forward-value-date-never-falls-on-holidays
  (for-all [params (gen/let [tenor        gen-tenor
                             trade-date   gen-date
                             fwd-date     (gen/return
                                           (let [{:keys [n unit]}
                                                 (settler/tenor tenor)]
                                             (.plus trade-date n unit)))
                             configs      (gen/map gen-currency
                                                   (gen-config {:start-date fwd-date})
                                                   {:num-elements 2})]
                     {:tenor      tenor
                      :trade-date trade-date
                      :configs    configs})]
    (let [{:keys [tenor trade-date configs]} params
          [ccy1 ccy2]   (keys configs)
          all-holidays  (into #{} (mapcat :holidays (vals configs)))
          vdate         (settler/value-date nil configs trade-date tenor
                                            ccy1 ccy2)]
      (not (some #{vdate} all-holidays)))))

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
        spot-date   (settler/spot nil nil trade-date "GHI" "JKL")]
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


(deftest tenor-returns-map-of-n-chronounits
  (let [units {"W" ChronoUnit/WEEKS
               "M" ChronoUnit/MONTHS
               "Y" ChronoUnit/YEARS}]
  (doseq [[n unit] (for [n    (range 1 12)
                         unit ["W" "M" "Y"]]
                     [n unit])]
    (testing (str "tenor " n unit)
      (is (= {:n n :unit (get units unit)}
             (settler/tenor (str n unit))))))))


(deftest forward-value-dates-are-always-between-two-chrono-units
  (let [units {"W" ChronoUnit/WEEKS
               "M" ChronoUnit/MONTHS
               "Y" ChronoUnit/YEARS}]
    (doseq [[[start end] unit-str] (for [start-end  (partition 2 1 (range 1 14))
                                         unit-str   (keys units)]
                                     [start-end unit-str])]
      (testing (str "date must between " start unit-str " and " end unit-str)
        (let [today (LocalDate/now)
              unit  (get units unit-str)
              vdate (settler/value-date nil nil today (str start unit-str)
                                        "ABC" "XYZ")]
          (is (and (.isAfter vdate  (.plus today start unit))
                   (.isBefore vdate (.plus today end unit)))))))))

(deftest forward-value-never-falls-on-weekends-when-configs-for-weekends-not-given
  (let [trade-date (LocalDate/of 2020 1 6)    ; Mon -> spot 2020-01-08 (Wed)
        value-date (settler/value-date nil nil trade-date "1M"
                                       "ABC" "XYZ") ; 2020-02-10 Mon, 8 Feb is Sat
        vday        (.getDayOfWeek value-date)]
    (is (not (some #{vday} #{DayOfWeek/SATURDAY DayOfWeek/SUNDAY})))))


(deftest forward-value-dates-falls-on-last-biz-day-when-spot-also-on-last-biz-day
  (doseq [[trade-date holidays tenor expected]
          [[(LocalDate/of 2020 1 29)    ;; trade -> Wed; spot -> Fri (31 Jan)
            nil
            "1M"
            (LocalDate/of 2020 2 28)]   ;; Fri
           [(LocalDate/of 2020 2 26)    ;; trade -> Wed; spot -> Fri (28 Feb)
            nil
            "1M"
            (LocalDate/of 2020 3 31)]   ;; Tue
           [(LocalDate/of 2020 2 26)    ;; trade -> Wed; spot -> Fri (28 Feb)
            nil
            "3M"
            (LocalDate/of 2020 5 29)]   ;; Fri
           [(LocalDate/of 2020 2 26)    ;; same scenario as above...
            #{(LocalDate/of 2020 5 29)} ;; except 29 May is a holiday
            "3M"
            (LocalDate/of 2020 5 28)]
          ]]
    (testing (str tenor " from " trade-date)
      (is (= expected
             (settler/value-date nil {"XYZ" {:holidays holidays}} trade-date
                                 tenor "ABC" "XYZ"))))))


(deftest forward-value-dates-spill-to-next-month-when-spot-is-not-last-day-and-tenor-is-week
  (is (= (LocalDate/of 2020 2 7)    ;; Fri
         (settler/value-date nil nil (LocalDate/of 2020 1 29) ;; Wed
                             "1W" "XYZ" "DEF")))

  (is (= (LocalDate/of 2020 5 1)    ;; Fri
         (settler/value-date nil nil (LocalDate/of 2020 4 22)  ;; Wed
                             "1W" "ABC" "XYZ")))

  (is (= (LocalDate/of 2020 6 1)    ;; Mon
         (settler/value-date nil
                             {"DEF" {:holidays #{(LocalDate/of 2020 5 29)}}} ;; Fri
                             (LocalDate/of 2020 5 6)  ;; Wed
                             "3W" "ABC" "DEF")))

  (is (= (LocalDate/of 2021 7 30)     ;; Fri
         (settler/value-date nil nil (LocalDate/of 2020 7 29)  ;; Wed
                             "1Y" "PQR" "STU"))))
