(ns calc.format-test
  (:require [clojure.test :refer [deftest testing is]]
            [calc.format :as fmt]))

;; ---------------------------------------------------------------------------
;; format-number — basic values
;; ---------------------------------------------------------------------------

(deftest format-number-integers
  (testing "integer values render without decimals"
    (is (= "42" (fmt/format-number 42)))
    (is (= "0" (fmt/format-number 0)))
    (is (= "-7" (fmt/format-number -7)))))

(deftest format-number-decimals
  (testing "floating point values render correctly"
    (is (= "3.14" (fmt/format-number 3.14 nil)))
    (is (= "-0.5" (fmt/format-number -0.5 nil)))))

(deftest format-number-large-integers
  (testing "integers above 2^32 are not truncated"
    (is (= "10000000000" (fmt/format-number 10000000000 nil)))
    (is (= "1099511627776" (fmt/format-number 1099511627776 nil)))
    ;; floats > 2^32: rounding should not truncate to 32 bits
    (is (= "10000000000" (fmt/format-number 10000000000.0 {:round 0}))))

  (testing "large integer as fraction"
    (is (= "10000000000" (fmt/format-number 10000000000.0 {:style :fraction})))))

;; ---------------------------------------------------------------------------
;; format-number — rounding / precision
;; ---------------------------------------------------------------------------

(deftest format-number-round
  (testing "round to 0 decimal places"
    (is (= "3" (fmt/format-number 3.14159 {:round 0}))))

  (testing "round to 2 decimal places"
    (is (= "3.14" (fmt/format-number 3.14159 {:round 2}))))

  (testing "round to 4 decimal places"
    (is (= "3.1416" (fmt/format-number 3.14159 {:round 4}))))

  (testing "round pads with zeros when needed"
    (is (= "3.00" (fmt/format-number 3 {:round 2}))))

  (testing "round negative number"
    (is (= "-2.72" (fmt/format-number -2.718 {:round 2})))))

;; ---------------------------------------------------------------------------
;; format-number — significant figures
;; ---------------------------------------------------------------------------

(deftest format-number-sig-figs
  (testing "1 sig fig"
    (is (= "3" (fmt/format-number 3.14159 {:sig-figs 1}))))

  (testing "3 sig figs"
    (is (= "3.14" (fmt/format-number 3.14159 {:sig-figs 3}))))

  (testing "6 sig figs"
    (is (= "3.14159" (fmt/format-number 3.14159 {:sig-figs 6}))))

  (testing "sig figs strips trailing zeros"
    (is (= "100" (fmt/format-number 100.0 {:sig-figs 3})))))

;; ---------------------------------------------------------------------------
;; format-number — fraction style
;; ---------------------------------------------------------------------------

(deftest format-number-fraction-style
  (testing "whole number as fraction"
    (is (= "5" (fmt/format-number 5.0 {:style :fraction}))))

  (testing "simple fraction"
    (is (= "1/2" (fmt/format-number 0.5 {:style :fraction}))))

  (testing "mixed fraction"
    (is (= "2 1/2" (fmt/format-number 2.5 {:style :fraction}))))

  (testing "negative fraction"
    (is (= "-1/4" (fmt/format-number -0.25 {:style :fraction}))))

  (testing "negative mixed fraction"
    (is (= "-3 1/2" (fmt/format-number -3.5 {:style :fraction}))))

  (testing "third as fraction"
    (is (= "1/3" (fmt/format-number (/ 1.0 3.0) {:style :fraction})))))

;; ---------------------------------------------------------------------------
;; format-number — JVM ratio display
;; ---------------------------------------------------------------------------

#?(:clj
   (deftest format-number-ratio
     (testing "ratio shows fraction = decimal"
       (let [result (fmt/format-number 1/5)]
         (is (= "1/5 = 0.2" result))))

     (testing "ratio with numeric option returns only decimal"
       (let [result (fmt/format-number 1/3 {:numeric true})]
         (is (not (re-find #"=" result)))
         (is (re-find #"^0\.\d+" result))))

     (testing "ratio with original-expr shows original = reduced = decimal"
       (let [result (fmt/format-number 2/10 {:original-expr "4 / 8"})]
         ;; 2/10 reduces to 1/5
         (is (re-find #"4 / 8" result))
         (is (re-find #"1/5" result))))

     (testing "ratio where original matches reduced shows fraction = decimal"
       (let [result (fmt/format-number 1/5 {:original-expr "1/5"})]
         (is (= "1/5 = 0.2" result))))))

;; ---------------------------------------------------------------------------
;; format-error
;; ---------------------------------------------------------------------------

(deftest format-error-known-types
  (testing "unknown unit"
    (is (= "Unknown unit: \"floop\"" (fmt/format-error {:error :unknown-unit :unit "floop"}))))

  (testing "unparseable"
    (is (= "Could not parse: \"blah\"" (fmt/format-error {:error :unparseable :phrase "blah"}))))

  (testing "ambiguous quantities"
    (is (= "Both sides of the conversion have quantities"
           (fmt/format-error {:error :ambiguous-quantities}))))

  (testing "incompatible dimensions"
    (is (= "Incompatible dimensions"
           (fmt/format-error {:error :incompatible-dimensions}))))

  (testing "unsupported operation"
    (is (= "Unsupported operation"
           (fmt/format-error {:error :unsupported-operation}))))

  (testing "invalid request"
    (is (= "Invalid request"
           (fmt/format-error {:error :invalid-request})))))

(deftest format-error-unknown-type
  (testing "unknown error type uses fallback"
    (let [result (fmt/format-error {:error :something-new})]
      (is (re-find #"Error:" result))
      (is (re-find #"something-new" result)))))
