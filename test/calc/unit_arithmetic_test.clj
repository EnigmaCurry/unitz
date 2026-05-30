(ns calc.unit-arithmetic-test
  (:require [clojure.test :refer [deftest testing is]]
            [calc.eval :as ev]
            [calc.cli :as cli]
            [calc.parser :as parser]))

(deftest parses-multi-unit-addition
  (testing "99 hours + 10 minutes + 2 seconds parses as qty-expr with :+ ops"
    (let [parsed (parser/parse-request "99 hours + 10 minutes + 2 seconds")]
      (is (= :convert (:op parsed)))
      (is (:qty-expr (:quantity parsed)))
      (is (= [:+ :+] (:ops (:quantity parsed))))
      (is (= 3 (count (:terms (:quantity parsed)))))
      (is (= :auto (:to parsed)))))

  (testing "2 feet + 6 inches parses as qty-expr"
    (let [parsed (parser/parse-request "2 feet + 6 inches")]
      (is (= :convert (:op parsed)))
      (is (:qty-expr (:quantity parsed)))
      (is (= [:+] (:ops (:quantity parsed))))))

  (testing "10 km - 500 meters parses with :- op"
    (let [parsed (parser/parse-request "10 km - 500 meters")]
      (is (= :convert (:op parsed)))
      (is (:qty-expr (:quantity parsed)))
      (is (= [:-] (:ops (:quantity parsed)))))))

(deftest evaluates-multi-unit-addition
  (testing "1 hour + 30 minutes in minutes = 90"
    (let [{:keys [result target]} (cli/process-request-text "1 hour + 30 minutes in minutes" nil)]
      (is (= "90" result))
      (is (= "min" target))))

  (testing "2 feet + 6 inches in inches = 30"
    (let [{:keys [result target]} (cli/process-request-text "2 feet + 6 inches in inches" nil)]
      (is (= "30" result))
      (is (= "in" target))))

  (testing "1 km + 500 m in meters = 1500"
    (let [{:keys [result target]} (cli/process-request-text "1 km + 500 m in meters" nil)]
      (is (= "1500" result))
      (is (= "m" target)))))

(deftest evaluates-multi-unit-subtraction
  (testing "10 km - 500 m in meters = 9500"
    (let [{:keys [result target]} (cli/process-request-text "10 km - 500 m in meters" nil)]
      (is (= "9500" result))
      (is (= "m" target))))

  (testing "2 hours - 30 minutes in minutes = 90"
    (let [{:keys [result target]} (cli/process-request-text "2 hours - 30 minutes in minutes" nil)]
      (is (= "90" result))
      (is (= "min" target)))))

(deftest evaluates-three-term-addition
  (testing "99 hours + 10 minutes + 2 seconds in seconds"
    (let [{:keys [result target]} (cli/process-request-text "99 hours + 10 minutes + 2 seconds in seconds" nil)]
      (is (= "357002" result))
      (is (= "s" target))))

  (testing "1 yard + 2 feet + 3 inches in inches = 63"
    (let [{:keys [result target]} (cli/process-request-text "1 yard + 2 feet + 3 inches in inches" nil)]
      (is (= "63" result))
      (is (= "in" target)))))

(deftest auto-scales-multi-unit-addition
  (testing "99 hours + 10 minutes + 2 seconds auto-selects a unit"
    (let [{:keys [result]} (cli/process-request-text "99 hours + 10 minutes + 2 seconds" nil)]
      (is (some? result))
      ;; Should produce something like "4.125... days" or "99.167... hours"
      (is (not (clojure.string/includes? result "Error"))))))

(deftest rejects-incompatible-unit-addition
  (testing "1 hour + 5 meters is incompatible"
    (let [{:keys [error]} (cli/process-request-text "1 hour + 5 meters in seconds" nil)]
      (is (some? error)))))
