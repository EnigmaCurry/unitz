(ns calc.units-test
  (:require [clojure.test :refer [deftest is testing]]
            [calc.core :as u]))

(deftest unit-registry-shape-test
  (testing "every unit has dim and scale"
    (doseq [[unit metadata] u/unit-defs]
      (is (contains? metadata :dim)
          (str unit " is missing :dim"))
      (is (contains? metadata :scale)
          (str unit " is missing :scale")))))

(deftest unit-registry-scale-test
  (testing "every unit scale is numeric"
    (doseq [[unit metadata] u/unit-defs]
      (is (number? (:scale metadata))
          (str unit " has non-numeric :scale")))))

(deftest derived-unit-dimension-test
  (testing "area units are length squared"
    (is (= {:length 2}
           (:dim (u/unit-spec :acre)))))

  (testing "volume units are length cubed"
    (is (= {:length 3}
           (:dim (u/unit-spec :l))))
    (is (= {:length 3}
           (:dim (u/unit-spec :gal)))))

  (testing "speed units have length over time dimension"
    (is (= {:length 1 :time -1}
           (:dim (u/unit-spec [:/ :m :s]))))))
