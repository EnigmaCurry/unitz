;; src/unitz/cli.clj
(ns unitz.cli
  (:require [unitz.core :as u]
            [clojure.string :as str]
            [unitz.parser :as parser])
  (:gen-class))

(defn format-number [x]
  (cond
    (instance? java.math.BigDecimal x)
    (.toPlainString (.stripTrailingZeros ^java.math.BigDecimal x))

    :else
    (str x)))

(defn process-request-text [input]
  (let [result (-> input
                   parser/parse-request
                   u/convert-request)]
    (if (and (map? result) (contains? result :error))
      result
      (format-number result))))

(defn usage []
  (str/join
   "\n"
   ["Usage:"
    "  unitz <request>"
    ""
    "Examples:"
    "  unitz 12 feet in yards"
    "  unitz 2 cubic yards to US liquid gallons"]))

(defn -main [& args]
  (let [input (str/trim (str/join " " args))]
    (if (str/blank? input)
      (do
        (binding [*out* *err*]
          (println (usage)))
        (System/exit 1))
      (try
        (println (process-request-text input))
        (catch Exception e
          (binding [*out* *err*]
            (println "Error:" (.getMessage e)))
          (System/exit 1))))))
