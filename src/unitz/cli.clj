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

(defn format-error [{:keys [error unit phrase] :as err}]
  (case error
    :unknown-unit (str "Error: Unknown unit \"" unit "\"")
    :unparseable (str "Error: Could not parse \"" phrase "\"")
    :ambiguous-quantities (str "Error: Both sides of the conversion have quantities")
    :incompatible-dimensions (str "Error: Incompatible dimensions")
    :unsupported-operation (str "Error: Unsupported operation")
    :invalid-request (str "Error: Invalid request")
    (str "Error: " (pr-str err))))

(defn process-request-text [input]
  (let [result (-> input
                   parser/parse-request
                   u/convert-request)]
    (if (and (map? result) (contains? result :error))
      {:error (format-error result)}
      {:result (format-number result)})))

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
  (let [input (str/trim (str/join " " args))
        tokens (str/split input #"\s+")
        verbose? (some #{"-v" "--verbose"} tokens)
        input (str/trim (str/join " " (remove #{"-v" "--verbose"} tokens)))]
    (if (str/blank? input)
      (do
        (binding [*out* *err*]
          (println (usage)))
        (System/exit 1))
      (try
        (let [{:keys [error result]} (process-request-text input)]
          (if error
            (do
              (binding [*out* *err*]
                (println error))
              (System/exit 1))
            (if verbose?
              (println (str input " = " result))
              (println result))))
        (catch Exception e
          (binding [*out* *err*]
            (println "Error:" (.getMessage e)))
          (System/exit 1))))))
