(ns calc.request-test
  (:require [clojure.test :refer [deftest testing is]]
            [calc.core :as u]
            [calc.cli :as cli]))

(deftest evaluates-simple-conversion-request
  (testing "basic scalar request"
    (is (= 4N
           (u/convert-request
            {:op :convert
             :quantity {:value 12N :unit :ft}
             :to :yd})))))

(deftest evaluates-temperature-conversion-request
  (testing "temperature conversions are affine, not just multiplicative"
    (is (= 0N
           (u/convert-request
            {:op :convert
             :quantity {:value 32N :unit :degF}
             :to :degC})))

    (is (= 212N
           (u/convert-request
            {:op :convert
             :quantity {:value 100N :unit :degC}
             :to :degF})))))

(deftest evaluates-compound-rate-request
  (testing "mph to km/hr"
    (is (= 96.56064M
           (u/convert-request
            {:op :convert
             :quantity {:value 60N :unit {:mi 1 :hr -1}}
             :to {:km 1 :hr -1}})))))

(deftest evaluates-area-request
  (testing "square feet to square meters"
    (is (= 0.9290304M
           (u/convert-request
            {:op :convert
             :quantity {:value 10N :unit {:ft 2}}
             :to {:m 2}})))))

(deftest evaluates-volume-request
  (testing "cubic yards to US liquid gallons"
    (is (= 403.94805194805195M
           (u/convert-request
            {:op :convert
             :quantity {:value 2N :unit {:yd 3}}
             :to :gal})))))

(deftest evaluates-mixed-quantity-request
  (testing "feet plus inches to centimeters"
    (is (= 180.34M
           (u/convert-request
            {:op :convert
             :quantity [{:value 5N :unit :ft}
                        {:value 11N :unit :in}]
             :to :cm}))))

  (testing "hours plus minutes to minutes"
    (is (= 90N
           (u/convert-request
            {:op :convert
             :quantity [{:value 1N :unit :hr}
                        {:value 30N :unit :min}]
             :to :min})))))

(deftest evaluates-quantity-arithmetic
  (testing "data / data-rate = time"
    (is (= 8N
           (u/convert-request
            {:op :convert
             :quantity {:qty-expr true
                        :terms [{:value 100N :unit :MB}
                                {:value 100N :unit {:Mb 1 :s -1}}]
                        :ops [:/]}
             :to :s}))))

  (testing "speed * time = distance"
    (is (= 120N
           (u/convert-request
            {:op :convert
             :quantity {:qty-expr true
                        :terms [{:value 60N :unit {:mi 1 :hr -1}}
                                {:value 2N :unit :hr}]
                        :ops [:*]}
             :to :mi}))))

  (testing "distance / time = speed"
    (is (= 50N
           (u/convert-request
            {:op :convert
             :quantity {:qty-expr true
                        :terms [{:value 100N :unit :km}
                                {:value 2N :unit :hr}]
                        :ops [:/]}
             :to {:km 1 :hr -1}}))))

  (testing "scalar multiply"
    (is (= 6.4M
           (u/convert-request
            {:op :convert
             :quantity {:qty-expr true
                        :terms [{:value 100N :unit :MB}
                                {:value 8N :unit {}}]
                        :ops [:*]}
             :to :Gb})))))

(deftest rejects-incompatible-dimensions
  (testing "length cannot convert to mass"
    (is (= {:error :incompatible-dimensions
            :from {:length 1}
            :to {:mass 1}}
           (u/convert-request
            {:op :convert
             :quantity {:value 12N :unit :ft}
             :to :lb}))))

  (testing "time cannot convert to length"
    (is (= {:error :incompatible-dimensions
            :from {:time 1}
            :to {:length 1}}
           (u/convert-request
            {:op :convert
             :quantity {:value 5N :unit :s}
             :to :m}))))

  (testing "length to temperature returns error, not exception"
    (is (= {:error :incompatible-dimensions
            :from {:length 1}
            :to {:temperature 1}}
           (u/convert-request
            {:op :convert
             :quantity {:value 5N :unit :m}
             :to :degC}))))

  (testing "temperature to length returns error, not exception"
    (is (= {:error :incompatible-dimensions
            :from {:temperature 1}
            :to {:length 1}}
           (u/convert-request
            {:op :convert
             :quantity {:value 100N :unit :degC}
             :to :m})))))

(deftest natural-language-formatting
  (testing "rounded to N decimals applies rounding"
    (let [{:keys [result target]} (cli/process-request-text "12 feet in yards rounded to 2 decimals" nil)]
      (is (= "4.00" result))
      (is (= "yards" target))))

  (testing "with N sig figs applies significant figures"
    (let [{:keys [result target]} (cli/process-request-text "5 miles in km with 3 sig figs" nil)]
      (is (= "8.05" result))
      (is (= "km" target))))

  (testing "CLI flags override parsed format"
    (let [{:keys [result]} (cli/process-request-text "12 feet in yards rounded to 2 decimals" {:round 4})]
      (is (= "4.0000" result))))

  (testing "no format clause leaves result unformatted"
    (let [{:keys [result]} (cli/process-request-text "12 feet in yards" nil)]
      (is (= "4" result)))))
