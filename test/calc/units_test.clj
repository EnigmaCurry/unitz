(ns calc.units-test
  (:require [clojure.test :refer [deftest is testing]]
            [calc.core :as u]))

(deftest unit-registry-shape-test
  (testing "every unit has kind, dimension, and factor"
    (doseq [[unit metadata] u/units]
      (is (contains? metadata :kind)
          (str unit " is missing :kind"))
      (is (contains? metadata :dimension)
          (str unit " is missing :dimension"))
      (is (contains? metadata :factor)
          (str unit " is missing :factor")))))

(deftest unit-registry-factor-test
  (testing "every unit factor is numeric"
    (doseq [[unit metadata] u/units]
      (is (number? (:factor metadata))
          (str unit " has non-numeric :factor")))))

(deftest derived-unit-dimension-test
  (testing "area units are length squared"
    (is (= {:length 2}
           (:dimension (u/unit :m2))))
    (is (= {:length 2}
           (:dimension (u/unit :ft2))))
    (is (= {:length 2}
           (:dimension (u/unit :acre)))))

  (testing "volume units are length cubed"
    (is (= {:length 3}
           (:dimension (u/unit :m3))))
    (is (= {:length 3}
           (:dimension (u/unit :liter))))
    (is (= {:length 3}
           (:dimension (u/unit :gal)))))

  (testing "speed units are length over time"
    (is (= {:length 1 :time -1}
           (:dimension (u/unit :mps))))
    (is (= {:length 1 :time -1}
           (:dimension (u/unit :mph))))
    (is (= {:length 1 :time -1}
           (:dimension (u/unit :kph))))))
