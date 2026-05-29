(ns calc.parser-test
  (:require [clojure.test :refer [deftest testing is are]]
            [calc.parser :as parser]))

(deftest clean-phrase-normalizes-whitespace
  (testing "collapses spaces around slash between words"
    (is (= "2 meters/second" (parser/clean-phrase "2 meters / second")))
    (is (= "2 meters/second" (parser/clean-phrase "2 meters/   second")))
    (is (= "2 meters/second" (parser/clean-phrase "2 meters/ second"))))

  (testing "collapses spaces around slash between digits"
    (is (= "21349/234234" (parser/clean-phrase "21349 /234234")))
    (is (= "21349/234234" (parser/clean-phrase "21349 / 234234")))
    (is (= "21349/234234" (parser/clean-phrase "21349/234234"))))

  (testing "does not collapse slash between unit and number"
    (is (= "100 MB / 100 Mbps" (parser/clean-phrase "100 MB / 100 Mbps")))))

(deftest parses-simple-scalar-conversions
  (testing "basic '<number> <unit> in/to <unit>' phrases"
    (are [phrase expected] (= expected (parser/parse-request phrase))

      "12 ft in yards"
      {:op :convert
       :quantity {:value 12N :unit :ft}
       :to :yd}

      "5 miles to km"
      {:op :convert
       :quantity {:value 5N :unit :mi}
       :to :km}

      "3 gallons in liters"
      {:op :convert
       :quantity {:value 3N :unit :gal}
       :to :l}

      "100 pounds to kilograms"
      {:op :convert
       :quantity {:value 100N :unit :lb}
       :to :kg})))

(deftest parses-unit-aliases-and-plurals
  (testing "aliases normalize before reaching the conversion engine"
    (are [phrase expected] (= expected (parser/parse-request phrase))

      "12 feet in yards"
      {:op :convert
       :quantity {:value 12N :unit :ft}
       :to :yd}

      "1 foot in inches"
      {:op :convert
       :quantity {:value 1N :unit :ft}
       :to :in}

      "2 meters to feet"
      {:op :convert
       :quantity {:value 2N :unit :m}
       :to :ft}

      "16 ounces in pounds"
      {:op :convert
       :quantity {:value 16N :unit :oz}
       :to :lb})))

(deftest parses-compact-input
  (testing "number and unit may be adjacent"
    (are [phrase expected] (= expected (parser/parse-request phrase))

      "12ft in yards"
      {:op :convert
       :quantity {:value 12N :unit :ft}
       :to :yd}

      "5km to miles"
      {:op :convert
       :quantity {:value 5N :unit :km}
       :to :mi}

      "100kg in lb"
      {:op :convert
       :quantity {:value 100N :unit :kg}
       :to :lb})))

(deftest parses-natural-language-question-forms
  (testing "common filler words do not affect the request"
    (are [phrase expected] (= expected (parser/parse-request phrase))

      "how many yards is 12 feet?"
      {:op :convert
       :quantity {:value 12N :unit :ft}
       :to :yd}

      "what is 12 ft in yards?"
      {:op :convert
       :quantity {:value 12N :unit :ft}
       :to :yd}

      "convert 12 feet to yards"
      {:op :convert
       :quantity {:value 12N :unit :ft}
       :to :yd}

      "how much is 5 kg in pounds?"
      {:op :convert
       :quantity {:value 5N :unit :kg}
       :to :lb})))

(deftest parses-reversed-how-many-form
  (testing "'how many target units are in value source unit' reverses target/source correctly"
    (are [phrase expected] (= expected (parser/parse-request phrase))

      "how many inches are in 3 feet?"
      {:op :convert
       :quantity {:value 3N :unit :ft}
       :to :in}

      "how many cups are in a gallon?"
      {:op :convert
       :quantity {:value 1N :unit :gal}
       :to :cup}

      "how many teaspoons are in 2 tablespoons?"
      {:op :convert
       :quantity {:value 2N :unit :tbsp}
       :to :tsp})))

(deftest parses-x-is-how-many-y-form
  (testing "'source quantity is how many target units' form"
    (are [phrase expected] (= expected (parser/parse-request phrase))

      "12 feet is how many yards?"
      {:op :convert
       :quantity {:value 12N :unit :ft}
       :to :yd}

      "100 kg is how many pounds?"
      {:op :convert
       :quantity {:value 100N :unit :kg}
       :to :lb})))

(deftest parses-temperature-conversions
  (testing "temperature still parses like scalar conversion; evaluator handles affine math"
    (are [phrase expected] (= expected (parser/parse-request phrase))

      "32 F in C"
      {:op :convert
       :quantity {:value 32N :unit :degF}
       :to :degC}

      "100 celsius to fahrenheit"
      {:op :convert
       :quantity {:value 100N :unit :degC}
       :to :degF}

      "273.15 K in C"
      {:op :convert
       :quantity {:value 273.15M :unit :K}
       :to :degC})))

(deftest parses-compound-rate-units
  (testing "compound units become exponent maps"
    (are [phrase expected] (= expected (parser/parse-request phrase))

      "60 miles per hour in km/h"
      {:op :convert
       :quantity {:value 60N :unit {:mi 1 :hr -1}}
       :to {:km 1 :hr -1}}

      "10 meters per second to mph"
      {:op :convert
       :quantity {:value 10N :unit {:m 1 :s -1}}
       :to {:mi 1 :hr -1}}

      "5 ft/s in m/s"
      {:op :convert
       :quantity {:value 5N :unit {:ft 1 :s -1}}
       :to {:m 1 :s -1}})))

(deftest parses-area-and-volume-units
  (testing "square/cubic modifiers become powers"
    (are [phrase expected] (= expected (parser/parse-request phrase))

      "10 square feet in square meters"
      {:op :convert
       :quantity {:value 10N :unit {:ft 2}}
       :to {:m 2}}

      "2 cubic yards in gallons"
      {:op :convert
       :quantity {:value 2N :unit {:yd 3}}
       :to :gal}

      "1 acre in square feet"
      {:op :convert
       :quantity {:value 1N :unit :acre}
       :to {:ft 2}})))

(deftest parses-derived-named-units
  (testing "named derived units may appear beside structural compound units"
    (are [phrase expected] (= expected (parser/parse-request phrase))

      "1 newton in kg m/s^2"
      {:op :convert
       :quantity {:value 1N :unit :N}
       :to {:kg 1 :m 1 :s -2}}

      "1 joule in newton meters"
      {:op :convert
       :quantity {:value 1N :unit :J}
       :to {:N 1 :m 1}}

      "1 watt in joules per second"
      {:op :convert
       :quantity {:value 1N :unit :W}
       :to {:J 1 :s -1}}

      "1 psi in pascals"
      {:op :convert
       :quantity {:value 1N :unit :psi}
       :to :Pa})))

(deftest parses-data-units
  (testing "data units preserve decimal/binary distinction"
    (are [phrase expected] (= expected (parser/parse-request phrase))

      "1024 bytes in kilobytes"
      {:op :convert
       :quantity {:value 1024N :unit :B}
       :to :KB}

      "1 GiB in MiB"
      {:op :convert
       :quantity {:value 1N :unit :GiB}
       :to :MiB}

      "1 megabit in megabytes"
      {:op :convert
       :quantity {:value 1N :unit :Mb}
       :to :MB}

      "100 Mbps in MB/s"
      {:op :convert
       :quantity {:value 100N :unit {:Mb 1 :s -1}}
       :to {:MB 1 :s -1}})))

(deftest parses-mixed-quantities
  (testing "multiple compatible quantity terms become a vector"
    (are [phrase expected] (= expected (parser/parse-request phrase))

      "1 hour 30 minutes in minutes"
      {:op :convert
       :quantity [{:value 1N :unit :hr}
                  {:value 30N :unit :min}]
       :to :min}

      "2 days 4 hours in hours"
      {:op :convert
       :quantity [{:value 2N :unit :day}
                  {:value 4N :unit :hr}]
       :to :hr}

      "5 feet 11 inches in cm"
      {:op :convert
       :quantity [{:value 5N :unit :ft}
                  {:value 11N :unit :in}]
       :to :cm}

      "6 lb 4 oz in grams"
      {:op :convert
       :quantity [{:value 6N :unit :lb}
                  {:value 4N :unit :oz}]
       :to :g})))

(deftest parses-fractions
  (testing "fractions remain exact ratios when possible"
    (are [phrase expected] (= expected (parser/parse-request phrase))

      "1/2 cup in tablespoons"
      {:op :convert
       :quantity {:value 1/2 :unit :cup}
       :to :tbsp}

      "3 1/2 inches in cm"
      {:op :convert
       :quantity {:value 7/2 :unit :in}
       :to :cm}

      "half a gallon in liters"
      {:op :convert
       :quantity {:value 1/2 :unit :gal}
       :to :l}

      "quarter mile in meters"
      {:op :convert
       :quantity {:value 1/4 :unit :mi}
       :to :m})))

(deftest parses-approximate-requests
  (testing "approximation intent is preserved for formatting"
    (are [phrase expected] (= expected (parser/parse-request phrase))

      "about 12 feet in meters"
      {:op :convert
       :approx? true
       :quantity {:value 12N :unit :ft}
       :to :m}

      "roughly 5 miles in km"
      {:op :convert
       :approx? true
       :quantity {:value 5N :unit :mi}
       :to :km}

      "~3 cups in ml"
      {:op :convert
       :approx? true
       :quantity {:value 3N :unit :cup}
       :to :ml})))

(deftest parses-formatting-requests
  (testing "formatting is separate from conversion semantics"
    (are [phrase expected] (= expected (parser/parse-request phrase))

      "12 ft in yards rounded to 2 decimals"
      {:op :convert
       :quantity {:value 12N :unit :ft}
       :to :yd
       :format {:round 2}}

      "5 miles in km with 3 sig figs"
      {:op :convert
       :quantity {:value 5N :unit :mi}
       :to :km
       :format {:sig-figs 3}}

      "1/3 meter in inches as a fraction"
      {:op :convert
       :quantity {:value 1/3 :unit :m}
       :to :in
       :format {:style :fraction}})))

(deftest parser-reports-unknown-units
  (testing "unknown units should produce data, not throw weird exceptions"
    (is (= {:error :unknown-unit
            :unit "blorps"
            :phrase "12 blorps in meters"}
           (parser/parse-request "12 blorps in meters")))))

(deftest parser-reports-ambiguous-quantities
  (testing "both sides having quantities produces a specific error"
    (is (= {:error :ambiguous-quantities
            :phrase "5 seconds in 102 years"}
           (parser/parse-request "5 seconds in 102 years")))))

(deftest parser-reports-unparseable-phrases
  (testing "nonsense input gets a useful parse error"
    (is (= {:error :unparseable
            :phrase "banana canoe surprise"}
           (parser/parse-request "banana canoe surprise")))))

(deftest parses-math-expressions
  (testing "parenthesised arithmetic in quantity"
    (are [phrase expected] (= expected (parser/parse-request phrase))

      "(2+2) cubic yards in gallons"
      {:op :convert
       :quantity {:value 4N :unit {:yd 3}}
       :to :gal}

      "(3*4) feet in meters"
      {:op :convert
       :quantity {:value 12N :unit :ft}
       :to :m}

      "(10-3) miles in km"
      {:op :convert
       :quantity {:value 7N :unit :mi}
       :to :km}

      "(100/4) pounds in kg"
      {:op :convert
       :quantity {:value 25N :unit :lb}
       :to :kg}))

  (testing "nested parentheses"
    (is (= {:op :convert
            :quantity {:value 20N :unit :ft}
            :to :m}
           (parser/parse-request "((2+3)*4) feet in meters"))))

  (testing "bare math without parentheses — no spaces"
    (are [phrase expected] (= expected (parser/parse-request phrase))

      "2+2 miles in km"
      {:op :convert
       :quantity {:value 4N :unit :mi}
       :to :km}

      "3*4 feet in inches"
      {:op :convert
       :quantity {:value 12N :unit :ft}
       :to :in}

      "10-3 pounds in kg"
      {:op :convert
       :quantity {:value 7N :unit :lb}
       :to :kg}))

  (testing "bare math with spaces between operators"
    (are [phrase expected] (= expected (parser/parse-request phrase))

      "2 + 2 miles in km"
      {:op :convert
       :quantity {:value 4N :unit :mi}
       :to :km}

      "3 * 4 feet in meters"
      {:op :convert
       :quantity {:value 12N :unit :ft}
       :to :m}

      "100 - 37 pounds in kg"
      {:op :convert
       :quantity {:value 63N :unit :lb}
       :to :kg}

      "2 + 3 + 5 gallons in liters"
      {:op :convert
       :quantity {:value 10N :unit :gal}
       :to :l}))

  (testing "operator precedence"
    (are [phrase expected] (= expected (parser/parse-request phrase))

      "(2+3*4) feet in meters"
      {:op :convert
       :quantity {:value 14N :unit :ft}
       :to :m}

      "(10-2*3) yards in feet"
      {:op :convert
       :quantity {:value 4N :unit :yd}
       :to :ft}))

  (testing "decimal arithmetic"
    (are [phrase expected] (= expected (parser/parse-request phrase))

      "(1.5+2.5) kg in pounds"
      {:op :convert
       :quantity {:value 4.0M :unit :kg}
       :to :lb}

      "(0.5*6) liters in gallons"
      {:op :convert
       :quantity {:value 3.0M :unit :l}
       :to :gal}))

  (testing "unary minus in parentheses"
    (is (= {:op :convert
            :quantity {:value 5N :unit :ft}
            :to :m}
           (parser/parse-request "(-2+7) feet in meters"))))

  (testing "division in parentheses"
    (are [phrase expected] (= expected (parser/parse-request phrase))

      "(12/4) feet in inches"
      {:op :convert
       :quantity {:value 3N :unit :ft}
       :to :in}

      "(100/3) meters in feet"
      {:op :convert
       :quantity {:value 100/3 :unit :m}
       :to :ft}))

  (testing "math with natural language forms"
    (are [phrase expected] (= expected (parser/parse-request phrase))

      "how many inches are in (2+1) feet?"
      {:op :convert
       :quantity {:value 3N :unit :ft}
       :to :in}

      "convert (5*2) km to miles"
      {:op :convert
       :quantity {:value 10N :unit :km}
       :to :mi}

      "what is (3+3) cups in ml?"
      {:op :convert
       :quantity {:value 6N :unit :cup}
       :to :ml}))

  (testing "exponentiation with ^"
    (are [phrase expected] (= expected (parser/parse-request phrase))

      "2^3 feet in inches"
      {:op :convert
       :quantity {:value 8N :unit :ft}
       :to :in}

      "(2^3) meters in feet"
      {:op :convert
       :quantity {:value 8N :unit :m}
       :to :ft}

      "3^2 yards in feet"
      {:op :convert
       :quantity {:value 9N :unit :yd}
       :to :ft}

      "2 ^ 4 pounds in kg"
      {:op :convert
       :quantity {:value 16N :unit :lb}
       :to :kg}))

  (testing "^ is right-associative and higher precedence than *"
    (are [phrase expected] (= expected (parser/parse-request phrase))

      ;; 2^3^2 = 2^9 = 512
      "(2^3^2) grams in kg"
      {:op :convert
       :quantity {:value 512N :unit :g}
       :to :kg}

      ;; 2*3^2 = 2*9 = 18
      "(2*3^2) feet in yards"
      {:op :convert
       :quantity {:value 18N :unit :ft}
       :to :yd}

      ;; 1+2^3 = 1+8 = 9
      "(1+2^3) miles in km"
      {:op :convert
       :quantity {:value 9N :unit :mi}
       :to :km}))

  (testing "math does not break plain numbers or fractions"
    (are [phrase expected] (= expected (parser/parse-request phrase))

      "42 feet in meters"
      {:op :convert
       :quantity {:value 42N :unit :ft}
       :to :m}

      "3 1/2 inches in cm"
      {:op :convert
       :quantity {:value 7/2 :unit :in}
       :to :cm}

      "1/2 cup in tablespoons"
      {:op :convert
       :quantity {:value 1/2 :unit :cup}
       :to :tbsp})))

(deftest parses-slash-unit-separator
  (testing "'/' works like 'per' in unit phrases"
    (are [phrase expected] (= expected (parser/parse-request phrase))

      "25 miles / hour in kph"
      {:op :convert
       :quantity {:value 25N :unit {:mi 1 :hr -1}}
       :to {:km 1 :hr -1}}

      "100 feet / second in mph"
      {:op :convert
       :quantity {:value 100N :unit {:ft 1 :s -1}}
       :to {:mi 1 :hr -1}})))

(deftest parses-quantity-arithmetic
  (testing "division of quantities: data / rate = time"
    (is (= {:op :convert
            :quantity {:qty-expr true
                       :terms [{:value 100N :unit :MB}
                               {:value 100N :unit {:Mb 1 :s -1}}]
                       :ops [:/]}
            :to :s}
           (parser/parse-request "100 MB / 100 Mbps in seconds"))))

  (testing "multiplication of quantities: speed * time = distance"
    (is (= {:op :convert
            :quantity {:qty-expr true
                       :terms [{:value 60N :unit {:mi 1 :hr -1}}
                               {:value 2N :unit :hr}]
                       :ops [:*]}
            :to :mi}
           (parser/parse-request "60 mph * 2 hours in miles"))))

  (testing "distance / time = speed"
    (is (= {:op :convert
            :quantity {:qty-expr true
                       :terms [{:value 100N :unit :km}
                               {:value 2N :unit :hr}]
                       :ops [:/]}
            :to {:km 1 :hr -1}}
           (parser/parse-request "100 km / 2 hours in kph"))))

  (testing "scalar-first arithmetic stays as plain math"
    ;; "3 * 4 feet" → scalar math, not qty arithmetic
    (is (= {:op :convert
            :quantity {:value 12N :unit :ft}
            :to :m}
           (parser/parse-request "3 * 4 feet in meters"))))

  (testing "unit-first with scalar second is qty arithmetic"
    (is (= {:op :convert
            :quantity {:qty-expr true
                       :terms [{:value 100N :unit :MB}
                               {:value 8N :unit {}}]
                       :ops [:*]}
            :to :Gb}
           (parser/parse-request "100 MB * 8 in Gb")))))
