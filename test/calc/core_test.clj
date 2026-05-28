(ns calc.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [calc.core :as u]))

(deftest unit-lookup-test
  (testing "known units resolve to metadata"
    (let [ft (u/unit :ft)]
      (is (= :length
             (:kind ft)))
      (is (= {:length 1}
             (:dimension ft)))
      (is (= 381/1250
             (:factor ft)))))

  (testing "unknown units throw"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Unknown unit"
         (u/unit :wat)))))

(deftest exponent-helper-test
  (testing "zero exponents are removed"
    (is (= {:length 2}
           (u/clean-exponents {:length 2 :time 0}))))

  (testing "exponents merge by addition"
    (is (= {:length 1 :time -1}
           (u/merge-exponents {:length 1}
                              {:time -1}))))

  (testing "opposite exponents cancel out"
    (is (= {}
           (u/merge-exponents {:length 1}
                              {:length -1})))))

(deftest unit-algebra-test
  (testing "dividing meters by seconds creates length over time"
    (is (= {:dimension {:length 1 :time -1}
            :factor 1}
           (u/divide-units (u/unit :m)
                           (u/unit :s)))))

  (testing "feet per second has expected resolved unit"
    (is (= {:dimension {:length 1 :time -1}
            :factor 381/1250}
           (u/divide-units (u/unit :ft)
                           (u/unit :s)))))

  (testing "miles per hour has expected resolved unit"
    (is (= {:dimension {:length 1 :time -1}
            :factor 1397/3125}
           (u/divide-units (u/unit :mile)
                           (u/unit :hour))))))

(deftest invalid-unit-expression-test
  (testing "invalid expression operator throws"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Unknown unit expression operator"
         (u/unit-expr [:+ :ft :s]))))

  (testing "invalid expression type throws"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Invalid unit expression"
         (u/unit-expr "ft")))))
