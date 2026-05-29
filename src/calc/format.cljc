(ns calc.format
  #?(:clj (:import [java.math BigDecimal MathContext RoundingMode]))
  (:require [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; Fraction formatting
;; ---------------------------------------------------------------------------

(defn- best-fraction
  "Find the simplest fraction approximating x using the Stern-Brocot mediant
   algorithm. Returns [numerator denominator] or nil if no good match found."
  [x max-denom tol]
  (loop [lo-n 0 lo-d 1 hi-n 1 hi-d 0 iters 0]
    (let [mid-n (+ lo-n hi-n)
          mid-d (+ lo-d hi-d)]
      (cond
        (> mid-d max-denom) nil
        (> iters 100) nil

        (< (#?(:clj #(Math/abs (double %)) :cljs js/Math.abs)
            (- (/ (double mid-n) (double mid-d)) x))
           tol)
        [mid-n mid-d]

        (< (/ (double mid-n) (double mid-d)) x)
        (recur mid-n mid-d hi-n hi-d (inc iters))

        :else
        (recur lo-n lo-d mid-n mid-d (inc iters))))))

(defn- format-mixed
  "Format numerator/denominator as a mixed number string."
  [n d neg?]
  (cond
    (= 1 d)
    (str (if neg? (- n) n))

    :else
    (let [whole (#?(:clj quot :cljs js/Math.trunc) n d)
          rem   (mod n d)
          s (cond
              (zero? rem)   (str whole)
              (zero? whole) (str n "/" d)
              :else         (str whole " " rem "/" d))]
      (if neg? (str "-" s) s))))

(defn- format-as-fraction [x]
  (let [d (double x)]
    (if (== d (#?(:clj long :cljs int) d))
      (str (#?(:clj long :cljs int) d))
      (let [neg?  (neg? d)
            abs-d (#?(:clj #(Math/abs %) :cljs js/Math.abs) d)
            whole (#?(:clj long :cljs js/Math.trunc) abs-d)
            frac  (- abs-d whole)]
        (if (< frac 1e-9)
          (str (if neg? (- whole) whole))
          (if-let [[n denom] (best-fraction frac 10000 1e-9)]
            (let [s (if (zero? whole)
                      (str n "/" denom)
                      (str whole " " n "/" denom))]
              (if neg? (str "-" s) s))
            ;; Fallback: no clean fraction found
            (str x)))))))

;; ---------------------------------------------------------------------------
;; Number formatting
;; ---------------------------------------------------------------------------

(defn format-number
  ([x] (format-number x nil))
  ([x {:keys [round sig-figs style numeric]}]
   (if (= :fraction style)
     (format-as-fraction x)
     #?(:clj
        (cond
          round
          (let [bd (if (instance? BigDecimal x) x (BigDecimal. (double x)))]
            (.toPlainString (.setScale bd (int round) RoundingMode/HALF_UP)))

          sig-figs
          (let [bd (if (instance? BigDecimal x) x (BigDecimal. (double x)))]
            (.toPlainString (.stripTrailingZeros
                             (.round bd (MathContext. (int sig-figs) RoundingMode/HALF_UP)))))

          :else
          (cond
            (instance? BigDecimal x)
            (.toPlainString (.stripTrailingZeros ^BigDecimal x))

            (ratio? x)
            (let [bd (BigDecimal. (double x))
                  approx (.toPlainString (.stripTrailingZeros
                                          (.round bd (MathContext. 10 RoundingMode/HALF_UP))))]
              (if numeric
                approx
                (str x " = " approx)))

            :else
            (str x)))

        :cljs
        (cond
          round
          (.toFixed (js/Number x) round)

          sig-figs
          (let [s (.toPrecision (js/Number x) sig-figs)]
            (if (str/includes? s ".")
              (-> s (str/replace #"0+$" "") (str/replace #"\.$" ""))
              s))

          (js/Number.isInteger x)
          (str (int x))

          :else
          (let [s (.toPrecision (js/Number x) 10)]
            (if (str/includes? s ".")
              (-> s
                  (str/replace #"0+$" "")
                  (str/replace #"\.$" ""))
              s)))))))

(defn format-error
  "Format an error map into a human-readable string (without 'Error: ' prefix)."
  [{:keys [error unit phrase]}]
  (case error
    :unknown-unit (str "Unknown unit: \"" unit "\"")
    :unparseable (str "Could not parse: \"" phrase "\"")
    :ambiguous-quantities "Both sides of the conversion have quantities"
    :incompatible-dimensions "Incompatible dimensions"
    :unsupported-operation "Unsupported operation"
    :invalid-request "Invalid request"
    (str "Error: " (pr-str {:error error}))))
