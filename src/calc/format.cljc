(ns calc.format
  #?(:clj (:import [java.math BigDecimal MathContext RoundingMode]))
  (:require [clojure.string :as str]))

(defn format-number
  ([x] (format-number x nil))
  ([x {:keys [round sig-figs]}]
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
            s))))))

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
