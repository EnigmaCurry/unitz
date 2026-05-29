(ns calc.request-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.string :as str]
            [calc.eval :as ev]
            [calc.cli :as cli]))

(deftest evaluates-simple-conversion-request
  (testing "basic scalar request"
    (let [result (ev/convert-request
                  {:op :convert
                   :quantity {:value 12N :unit :ft}
                   :to :yd})]
      (is (:ok? result))
      (is (= 4N (:value result))))))

(deftest evaluates-temperature-conversion-request
  (testing "temperature conversions are affine, not just multiplicative"
    (let [r1 (ev/convert-request
              {:op :convert
               :quantity {:value 32N :unit :degF}
               :to :degC})
          r2 (ev/convert-request
              {:op :convert
               :quantity {:value 100N :unit :degC}
               :to :degF})]
      (is (:ok? r1))
      (is (= 0N (:value r1)))
      (is (:ok? r2))
      (is (= 212N (:value r2))))))

(deftest evaluates-compound-rate-request
  (testing "mph to km/hr"
    (let [result (ev/convert-request
                  {:op :convert
                   :quantity {:value 60N :unit {:mi 1 :hr -1}}
                   :to {:km 1 :hr -1}})]
      (is (:ok? result))
      (is (= 96.56064M (:value result))))))

(deftest evaluates-area-request
  (testing "square feet to square meters"
    (let [result (ev/convert-request
                  {:op :convert
                   :quantity {:value 10N :unit {:ft 2}}
                   :to {:m 2}})]
      (is (:ok? result))
      (is (= 0.9290304M (:value result))))))

(deftest evaluates-volume-request
  (testing "cubic yards to US liquid gallons"
    (let [result (ev/convert-request
                  {:op :convert
                   :quantity {:value 2N :unit {:yd 3}}
                   :to :gal})]
      (is (:ok? result))
      (is (= 403.94805194805195M (:value result))))))

(deftest evaluates-mixed-quantity-request
  (testing "feet plus inches to centimeters"
    (let [result (ev/convert-request
                  {:op :convert
                   :quantity [{:value 5N :unit :ft}
                              {:value 11N :unit :in}]
                   :to :cm})]
      (is (:ok? result))
      (is (= 180.34M (:value result)))))

  (testing "hours plus minutes to minutes"
    (let [result (ev/convert-request
                  {:op :convert
                   :quantity [{:value 1N :unit :hr}
                              {:value 30N :unit :min}]
                   :to :min})]
      (is (:ok? result))
      (is (= 90N (:value result))))))

(deftest evaluates-quantity-arithmetic
  (testing "data / data-rate = time"
    (let [result (ev/convert-request
                  {:op :convert
                   :quantity {:qty-expr true
                              :terms [{:value 100N :unit :MB}
                                      {:value 100N :unit {:Mb 1 :s -1}}]
                              :ops [:/]}
                   :to :s})]
      (is (:ok? result))
      (is (= 8N (:value result)))))

  (testing "speed * time = distance"
    (let [result (ev/convert-request
                  {:op :convert
                   :quantity {:qty-expr true
                              :terms [{:value 60N :unit {:mi 1 :hr -1}}
                                      {:value 2N :unit :hr}]
                              :ops [:*]}
                   :to :mi})]
      (is (:ok? result))
      (is (= 120N (:value result)))))

  (testing "distance / time = speed"
    (let [result (ev/convert-request
                  {:op :convert
                   :quantity {:qty-expr true
                              :terms [{:value 100N :unit :km}
                                      {:value 2N :unit :hr}]
                              :ops [:/]}
                   :to {:km 1 :hr -1}})]
      (is (:ok? result))
      (is (= 50N (:value result)))))

  (testing "scalar multiply"
    (let [result (ev/convert-request
                  {:op :convert
                   :quantity {:qty-expr true
                              :terms [{:value 100N :unit :MB}
                                      {:value 8N :unit {}}]
                              :ops [:*]}
                   :to :Gb})]
      (is (:ok? result))
      (is (= 6.4M (:value result))))))

(deftest evaluates-percentage-expressions
  (testing "what percent: 10 is what percent of 100 = 10%"
    (let [{:keys [result]} (cli/process-request-text "10 is what percent of 100" nil)]
      (is (= "10%" result))))

  (testing "what percent: 1 is what percent of 3"
    (let [{:keys [result]} (cli/process-request-text "1 is what percent of 3" nil)]
      (is (str/starts-with? result "33.3333"))))

  (testing "percent of: 15 percent of 50 = 7.5"
    (let [{:keys [result]} (cli/process-request-text "15 percent of 50" nil)]
      (is (= "7.5" result))))

  (testing "percent of: 50 percent of 200 = 100"
    (let [{:keys [result]} (cli/process-request-text "50 percent of 200" nil)]
      (is (= "100" result))))

  (testing "percent sign syntax: 15% of 50 = 7.5"
    (let [{:keys [result]} (cli/process-request-text "15% of 50" nil)]
      (is (= "7.5" result))))

  (testing "reversed form: what percent of 100 is 25 = 25%"
    (let [{:keys [result]} (cli/process-request-text "what percent of 100 is 25" nil)]
      (is (= "25%" result))))

  (testing "what is X percent of Y form"
    (let [{:keys [result]} (cli/process-request-text "what is 10 percent of 100" nil)]
      (is (= "10" result)))))

(deftest rejects-incompatible-dimensions
  (testing "length cannot convert to mass"
    (let [result (ev/convert-request
                  {:op :convert
                   :quantity {:value 12N :unit :ft}
                   :to :lb})]
      (is (not (:ok? result)))
      (is (= :incompatible-dimensions (:error result)))
      (is (= {:length 1} (:from result)))
      (is (= {:mass 1} (:to result)))))

  (testing "time cannot convert to length"
    (let [result (ev/convert-request
                  {:op :convert
                   :quantity {:value 5N :unit :s}
                   :to :m})]
      (is (not (:ok? result)))
      (is (= :incompatible-dimensions (:error result)))
      (is (= {:time 1} (:from result)))
      (is (= {:length 1} (:to result)))))

  (testing "length to temperature returns error, not exception"
    (let [result (ev/convert-request
                  {:op :convert
                   :quantity {:value 5N :unit :m}
                   :to :degC})]
      (is (not (:ok? result)))
      (is (= :incompatible-dimensions (:error result)))))

  (testing "temperature to length returns error, not exception"
    (let [result (ev/convert-request
                  {:op :convert
                   :quantity {:value 100N :unit :degC}
                   :to :m})]
      (is (not (:ok? result)))
      (is (= :incompatible-dimensions (:error result))))))

(deftest natural-language-formatting
  (testing "rounded to N decimals applies rounding"
    (let [{:keys [result target]} (cli/process-request-text "12 feet in yards rounded to 2 decimals" nil)]
      (is (= "4.00" result))
      (is (= "yd" target))))

  (testing "with N sig figs applies significant figures"
    (let [{:keys [result target]} (cli/process-request-text "5 miles in km with 3 sig figs" nil)]
      (is (= "8.05" result))
      (is (= "km" target))))

  (testing "CLI flags override parsed format"
    (let [{:keys [result]} (cli/process-request-text "12 feet in yards rounded to 2 decimals" {:round 4})]
      (is (= "4.0000" result))))

  (testing "no format clause leaves result unformatted"
    (let [{:keys [result]} (cli/process-request-text "12 feet in yards" nil)]
      (is (= "4" result))))

  (testing "as a fraction formats exact results"
    (let [{:keys [result target]} (cli/process-request-text "7 inches in feet as a fraction" nil)]
      (is (= "7/12" result))
      (is (= "ft" target))))

  (testing "as a fraction with integer result omits denominator"
    (let [{:keys [result target]} (cli/process-request-text "1 yard in feet as a fraction" nil)]
      (is (= "3" result))
      (is (= "ft" target))))

  (testing "as a fraction uses mixed number form"
    (let [{:keys [result target]} (cli/process-request-text "5 feet 11 inches in cm as a fraction" nil)]
      (is (= "180 17/50" result))
      (is (= "cm" target)))))

(deftest scalar-math-preserves-unit
  (testing "12 * 4 days = 48 days"
    (let [{:keys [result]} (cli/process-request-text "12 * 4 days" nil)]
      (is (= "48 days" result))))
  (testing "3 + 2 hours = 5 hours"
    (let [{:keys [result]} (cli/process-request-text "3 + 2 hours" nil)]
      (is (= "5 hours" result))))
  (testing "scalar math with explicit target still works"
    (let [{:keys [result from target]} (cli/process-request-text "12 * 4 days in seconds" nil)]
      (is (= "4147200" result))
      (is (= "s" target)))))

(deftest scalar-math-with-compound-units
  (testing "12 * 4 miles/hour = 48 mi/hr"
    (let [{:keys [result]} (cli/process-request-text "12 * 4 miles/hour" nil)]
      (is (= "48 mi/hr" result))))
  (testing "3 * 5 km/hr"
    (let [{:keys [result]} (cli/process-request-text "3 * 5 km/hr" nil)]
      (is (= "250 m/min" result))))
  (testing "2 + 3 feet/second"
    (let [{:keys [result]} (cli/process-request-text "2 + 3 feet/second" nil)]
      (is (= "91.44 m/min" result))))
  (testing "scalar math with compound unit and explicit target"
    (let [{:keys [result target]} (cli/process-request-text "12 * 4 miles/hour in km/hr" nil)]
      (is (= "77.248512" result))
      (is (= "km/hr" target)))))

(deftest standalone-compound-unit-quantity
  (testing "standalone compound unit without conversion"
    (let [result (ev/convert-request
                  {:op :convert
                   :quantity {:value 60N :unit {:mi 1 :hr -1}}
                   :to :auto})]
      (is (:ok? result))
      (is (some? (:unit-label result)))))
  (testing "standalone simple unit still works"
    (let [result (ev/convert-request
                  {:op :convert
                   :quantity {:value 12N :unit :ft}
                   :to :auto})]
      (is (:ok? result))
      (is (= 12N (:value result)))
      (is (= "feet" (:unit-label result))))))

(deftest compound-unit-syntax
  (testing "ft*ft parsed as square feet and converts to sq yards"
    (let [{:keys [result target]} (cli/process-request-text "1 ft*ft in sq yd" nil)]
      (is (some? result))
      (is (= "yd^2" target))))

  (testing "60 mph in ft/s"
    (let [{:keys [result target]} (cli/process-request-text "60 mph in ft/s" nil)]
      (is (= "88" result))
      (is (= "ft/s" target))))

  (testing "m*m parsed as square meters"
    (let [{:keys [result target]} (cli/process-request-text "9 m*m in sq ft" nil)]
      (is (some? result))
      (is (= "ft^2" target))))

  (testing "sq ft as alias for square feet"
    (let [{:keys [result target]} (cli/process-request-text "1 sq ft in sq m" nil)]
      (is (some? result))
      (is (= "m^2" target))))

  (testing "squared yard (prefix form)"
    (let [{:keys [result target]} (cli/process-request-text "1 ft*ft in squared yard" nil)]
      (is (some? result))
      (is (= "yd^2" target))))

  (testing "yard square (suffix form)"
    (let [{:keys [result target]} (cli/process-request-text "1 ft*ft in yard square" nil)]
      (is (some? result))
      (is (= "yd^2" target))))

  (testing "yard squared (suffix form)"
    (let [{:keys [result target]} (cli/process-request-text "1 ft*ft in yard squared" nil)]
      (is (some? result))
      (is (= "yd^2" target))))

  (testing "cu ft as alias for cubic feet"
    (let [{:keys [result target]} (cli/process-request-text "1 cu ft in cu m" nil)]
      (is (some? result))
      (is (= "m^3" target))))

  (testing "foot cubed (suffix form)"
    (let [{:keys [result target]} (cli/process-request-text "1 foot cubed in meter cubed" nil)]
      (is (some? result))
      (is (= "m^3" target))))

  (testing "sqft shorthand"
    (let [{:keys [result]} (cli/process-request-text "12 sqft" nil)]
      (is (= "12 ft^2" result))))

  (testing "sqft to sqm conversion"
    (let [{:keys [result target]} (cli/process-request-text "100 sqft in sqm" nil)]
      (is (some? result))
      (is (= "m^2" target))))

  (testing "cuft shorthand"
    (let [{:keys [result]} (cli/process-request-text "1 cuft" nil)]
      (is (= "1 ft^3" result)))))
