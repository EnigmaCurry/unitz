(ns calc.invariant-test
  (:require [clojure.test :refer [deftest is testing]]
            [calc.core :as u]
            [calc.parser :as p]))

;; ---------------------------------------------------------------------------
;; Identity: converting a unit to itself returns the same value
;; ---------------------------------------------------------------------------

(deftest identity-conversion-test
  (testing "converting any unit to itself returns the original value"
    (doseq [[unit-key metadata] u/unit-defs
            :when (not (:temperature metadata))]
      (is (== 42 (u/convert-scalar 42 unit-key unit-key))
          (str unit-key " -> " unit-key " should be identity")))))

(deftest identity-temperature-test
  (testing "converting a temperature unit to itself returns the original value"
    (doseq [unit [:degF :degC :K]]
      (is (== 100 (u/convert-temperature 100 unit unit))
          (str unit " -> " unit " should be identity")))))

;; ---------------------------------------------------------------------------
;; Round-trip: A->B->A returns the original value
;; ---------------------------------------------------------------------------

(def ^:private round-trip-pairs
  "Representative unit pairs from each dimension for round-trip testing."
  [;; Length
   [:ft :m] [:mi :km] [:in :cm] [:yd :m]
   ;; Mass
   [:kg :lb] [:oz :g] [:tonne :ton]
   ;; Time
   [:hr :min] [:day :s] [:week :day] [:yr :day]
   ;; Volume
   [:l :gal] [:ml :floz] [:cup :ml] [:qt :l]
   ;; Area
   [:acre :ha]
   ;; Data
   [:KB :KiB] [:MB :GB] [:GiB :GB]
   ;; Energy
   [:J :cal] [:kcal :kWh] [:BTU :J]
   ;; Power
   [:W :kW] [:MW :GW]
   ;; Pressure
   [:Pa :psi] [:bar :atm]
   ;; Frequency
   [:Hz :kHz] [:MHz :GHz]
   ;; Angle
   [:rad :deg]])

(deftest round-trip-conversion-test
  (testing "converting A->B->A returns original value (within tolerance)"
    (doseq [[a b] round-trip-pairs]
      (let [original 123.456
            there (u/convert-scalar original a b)
            back  (u/convert-scalar there b a)]
        (is (not (u/error? there))
            (str a " -> " b " failed: " (pr-str there)))
        (when-not (u/error? there)
          (is (< (abs (- (double back) (double original))) 0.001)
              (str a " -> " b " -> " a ": expected " original
                   " but got " back)))))))

(deftest round-trip-temperature-test
  (testing "temperature round-trips preserve value"
    (doseq [[a b] [[:degF :degC] [:degC :K] [:degF :K]]]
      (let [original 72.0
            there (u/convert-temperature original a b)
            back  (u/convert-temperature there b a)]
        (is (< (abs (- (double back) (double original))) 0.001)
            (str a " -> " b " -> " a ": expected " original
                 " but got " back))))))

;; ---------------------------------------------------------------------------
;; Dimensional compatibility is symmetric
;; ---------------------------------------------------------------------------

(deftest compatibility-symmetry-test
  (testing "compatible? is symmetric: compatible?(A,B) == compatible?(B,A)"
    (let [units (keys u/unit-defs)
          non-temp (remove #(:temperature (get u/unit-defs %)) units)
          ;; Test a sample of pairs rather than O(n^2) full cross product
          sample-pairs (take 200 (for [a non-temp
                                       b non-temp
                                       :when (not= a b)]
                                   [a b]))]
      (doseq [[a b] sample-pairs]
        (is (= (u/compatible? a b) (u/compatible? b a))
            (str "compatible? is not symmetric for " a " and " b))))))

;; ---------------------------------------------------------------------------
;; Every alias parses to a known unit
;; ---------------------------------------------------------------------------

(deftest every-alias-resolves-test
  (testing "every alias in unit-defs resolves through unit-aliases"
    (doseq [[unit-key {:keys [aliases]}] u/unit-defs
            alias aliases]
      (is (contains? u/unit-aliases alias)
          (str "alias \"" alias "\" for " unit-key
               " not found in unit-aliases"))
      (is (= unit-key (get u/unit-aliases alias))
          (str "alias \"" alias "\" resolves to "
               (get u/unit-aliases alias)
               " instead of " unit-key)))))

;; ---------------------------------------------------------------------------
;; Every non-temperature unit has a valid positive scale
;; ---------------------------------------------------------------------------

(deftest all-scales-positive-test
  (testing "every non-temperature unit has a positive numeric scale"
    (doseq [[unit-key metadata] u/unit-defs
            :when (not (:temperature metadata))]
      (is (number? (:scale metadata))
          (str unit-key " scale is not a number"))
      (is (pos? (:scale metadata))
          (str unit-key " has non-positive scale: " (:scale metadata))))))

;; ---------------------------------------------------------------------------
;; Error paths return data, not thrown exceptions
;; ---------------------------------------------------------------------------

(deftest incompatible-conversion-returns-error-map
  (testing "converting incompatible units returns an error map, not an exception"
    (let [result (u/convert-scalar 1 :ft :s)]
      (is (map? result))
      (is (= :incompatible-dimensions (:error result))))))

(deftest temperature-incompatible-returns-error-map
  (testing "converting temperature to non-temperature returns error map"
    (let [result (u/convert-temperature 100 :degF :ft)]
      (is (map? result))
      (is (= :incompatible-dimensions (:error result))))))

(deftest convert-request-error-paths
  (testing "convert-request with bad input returns error maps"
    (let [unknown (p/parse-request "42 zorblax in feet")]
      (is (map? unknown))
      (is (contains? unknown :error)))))

(deftest unsupported-op-returns-error-map
  (testing "convert-request with unsupported op returns error"
    (let [result (u/convert-request {:op :something-else :quantity {:value 1 :unit :ft} :to :m})]
      (is (map? result))
      (is (= :unsupported-operation (:error result))))))
