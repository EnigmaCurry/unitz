(ns calc.conversion-test
  (:require [clojure.test :refer [deftest is testing]]
            [calc.core :as u]))

(deftest compatibility-test
  (testing "simple compatible units"
    (is (true? (u/compatible? :ft :yd))))

  (testing "simple incompatible units"
    (is (false? (u/compatible? :ft :s))))

  (testing "compound compatible units"
    (is (true? (u/compatible? [:/ :mile :hour]
                              [:/ :ft :s]))))

  (testing "compound incompatible units"
    (is (false? (u/compatible? [:/ :mile :hour]
                               :ft)))))

(deftest simple-conversion-test
  (testing "feet to yards"
    (is (= 4N
           (u/convert 12 :ft :yd))))

  (testing "hours to minutes"
    (is (= 60
           (u/convert 1 :hour :min)))))

(deftest compound-conversion-test
  (testing "miles per hour to feet per second"
    (is (= 88N
           (u/convert 60 [:/ :mile :hour] [:/ :ft :s]))))

  (testing "square yards to square feet"
    (is (= 9N
           (u/convert 1 [:* :yd :yd] [:* :ft :ft])))))

(deftest resolved-unit-conversion-test
  (testing "convert between already-resolved compound units"
    (let [ft-per-second (u/divide-units (u/unit :ft)
                                        (u/unit :s))
          miles-per-hour (u/divide-units (u/unit :mile)
                                         (u/unit :hour))]
      (is (= 88N
             (u/convert-resolved-units 60
                                       miles-per-hour
                                       ft-per-second))))))

(deftest invalid-conversion-test
  (testing "incompatible conversions throw"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Incompatible units"
         (u/convert 1 :ft :s)))))

(deftest length-conversion-test
  (testing "feet to yards"
    (is (= 4N
           (u/convert 12 :ft :yd))))

  (testing "miles to feet"
    (is (= 5280N
           (u/convert 1 :mile :ft)))))

(deftest mass-conversion-test
  (testing "kilograms to grams"
    (is (= 1000N
           (u/convert 1 :kg :g))))

  (testing "pounds to ounces"
    (is (= 16N
           (u/convert 1 :lb :oz)))))

(deftest time-conversion-test
  (testing "hours to minutes"
    (is (= 60
           (u/convert 1 :hour :min))))

  (testing "days to hours"
    (is (= 24
           (u/convert 1 :day :hour)))))

(deftest area-conversion-test
  (testing "square yards to square feet"
    (is (= 9N
           (u/convert 1 [:* :yd :yd] [:* :ft :ft]))))

  (testing "named square feet to compound square feet"
    (is (= 1
           (u/convert 1 :ft2 [:* :ft :ft])))))

(deftest volume-conversion-test
  (testing "liters to cubic meters"
    (is (= 1/1000
           (u/convert 1 :liter :m3))))

  (testing "milliliters to liters"
    (is (= 1/1000
           (u/convert 1 :ml :liter)))))

(deftest speed-conversion-test
  (testing "miles per hour to feet per second"
    (is (= 88N
           (u/convert 60 [:/ :mile :hour] [:/ :ft :s]))))

  (testing "named mph to compound miles per hour"
    (is (= 1
           (u/convert 1 :mph [:/ :mile :hour]))))

  (testing "meters per second to named mps"
    (is (= 1
           (u/convert 1 [:/ :m :s] :mps))))

  (testing "kph to mps"
    (is (= 5/18
           (u/convert 1 :kph :mps))))

  (testing "mph to kph"
    (is (= 301752/3125
           (u/convert 60 :mph :kph)))))
