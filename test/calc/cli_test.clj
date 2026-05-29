(ns calc.cli-test
  (:require [clojure.test :refer [deftest testing is]]
            [calc.cli :as cli]))

(deftest math-expressions-via-process-request-text
  (testing "bare number"
    (let [{:keys [result error]} (cli/process-request-text "2" nil)]
      (is (nil? error))
      (is (= "2" result))))

  (testing "simple addition"
    (let [{:keys [result error]} (cli/process-request-text "2+2" nil)]
      (is (nil? error))
      (is (= "4" result))))

  (testing "addition with spaces"
    (let [{:keys [result error]} (cli/process-request-text "2 + 2" nil)]
      (is (nil? error))
      (is (= "4" result))))

  (testing "multiplication"
    (let [{:keys [result error]} (cli/process-request-text "3 * 4" nil)]
      (is (nil? error))
      (is (= "12" result))))

  (testing "parenthesized expression"
    (let [{:keys [result error]} (cli/process-request-text "3 * (4 + 5)" nil)]
      (is (nil? error))
      (is (= "27" result))))

  (testing "subtraction"
    (let [{:keys [result error]} (cli/process-request-text "10 - 3" nil)]
      (is (nil? error))
      (is (= "7" result))))

  (testing "division"
    (let [{:keys [result error]} (cli/process-request-text "10 / 2" nil)]
      (is (nil? error))
      (is (= "5" result))))

  (testing "decimal result"
    (let [{:keys [result error]} (cli/process-request-text "1 / 3" nil)]
      (is (nil? error))
      (is (some? result))))

  (testing "non-terminating decimal division does not throw"
    (let [{:keys [result error]} (cli/process-request-text "(2+2.5) / 77" nil)]
      (is (nil? error))
      (is (some? result))))

  (testing "decimal division with repeating result"
    (let [{:keys [result error]} (cli/process-request-text "10.0 / 3" nil)]
      (is (nil? error))
      (is (some? result)))))

(deftest math-does-not-break-unit-conversions
  (testing "unit conversion still works"
    (let [{:keys [result from target error]} (cli/process-request-text "12 feet in yards" nil)]
      (is (nil? error))
      (is (= "12 feet" from))
      (is (= "yards" target))
      (is (= "4" result))))

  (testing "temperature conversion still works"
    (let [{:keys [result error]} (cli/process-request-text "100 celsius in fahrenheit" nil)]
      (is (nil? error))
      (is (= "212" result)))))

(deftest math-with-format-opts
  (testing "precision applied to math result"
    (let [{:keys [result error]} (cli/process-request-text "1 / 3" {:round 2})]
      (is (nil? error))
      (is (= "0.33" result)))))

(deftest display-normalizes-spacing
  (testing "slash with trailing space"
    (let [{:keys [from error]} (cli/process-request-text "2 meters/ second in kph" nil)]
      (is (nil? error))
      (is (= "2 meters/second" from))))

  (testing "slash with surrounding spaces"
    (let [{:keys [from error]} (cli/process-request-text "2 meters / second in kph" nil)]
      (is (nil? error))
      (is (= "2 meters/second" from))))

  (testing "slash with excessive spaces"
    (let [{:keys [from error]} (cli/process-request-text "2 meters/   second in kph" nil)]
      (is (nil? error))
      (is (= "2 meters/second" from))))

  (testing "already compact slash is unchanged"
    (let [{:keys [from error]} (cli/process-request-text "2 meters/second in kph" nil)]
      (is (nil? error))
      (is (= "2 meters/second" from)))))
