;; src/unitz/cli.clj
(ns unitz.cli
  (:require [unitz.core :as u]
            [clojure.string :as str]
            [unitz.parser :as parser])
  (:gen-class))

(defn format-number
  ([x] (format-number x nil))
  ([x precision]
   (if precision
     (let [bd (if (instance? java.math.BigDecimal x)
                x
                (BigDecimal. (double x)))]
       (.toPlainString (.setScale bd (int precision) java.math.RoundingMode/HALF_UP)))
     (cond
       (instance? java.math.BigDecimal x)
       (.toPlainString (.stripTrailingZeros ^java.math.BigDecimal x))

       :else
       (str x)))))

(defn format-error [{:keys [error unit phrase] :as err}]
  (case error
    :unknown-unit (str "Error: Unknown unit \"" unit "\"")
    :unparseable (str "Error: Could not parse \"" phrase "\"")
    :ambiguous-quantities (str "Error: Both sides of the conversion have quantities")
    :incompatible-dimensions (str "Error: Incompatible dimensions")
    :unsupported-operation (str "Error: Unsupported operation")
    :invalid-request (str "Error: Invalid request")
    (str "Error: " (pr-str err))))

(defn split-input-parts [input]
  (when-let [[_ lhs rhs] (re-find #"(?i)^(.+?)\s+(?:in|to)\s+(.+?)$" input)]
    [(str/trim lhs) (str/trim rhs)]))

(defn process-request-text [input precision]
  (let [parsed (parser/parse-request input)
        result (u/convert-request parsed)]
    (if (and (map? result) (contains? result :error))
      {:error (format-error result)}
      (let [[lhs rhs] (split-input-parts input)
            has-number? #(re-find #"\d|^(?:a|an|one|two|three|four|five|six|seven|eight|nine|ten|eleven|twelve|thirteen|fourteen|fifteen|sixteen|seventeen|eighteen|nineteen|twenty|thirty|forty|fifty|sixty|seventy|eighty|ninety|hundred|thousand|million|billion|half|quarter)\b" (str/lower-case (or % "")))
            swapped? (and (not (has-number? lhs)) (has-number? rhs))]
        {:result (format-number result precision)
         :from (if swapped? rhs lhs)
         :target (if swapped? lhs rhs)}))))

(defn usage []
  (str/join
   "\n"
   ["Usage:"
    "  unitz <request>"
    ""
    "Examples:"
    "  unitz 12 feet in yards"
    "  unitz 2 cubic yards to US liquid gallons"]))

(defn parse-precision [tokens]
  (loop [remaining tokens
         precision nil
         result []]
    (if (empty? remaining)
      [precision result]
      (let [t (first remaining)]
        (if (and (or (= "-p" t) (= "--precision" t))
                 (second remaining))
          (recur (drop 2 remaining)
                 (Long/parseLong (second remaining))
                 result)
          (recur (rest remaining) precision (conj result t)))))))

(defn -main [& args]
  (let [input (str/trim (str/join " " args))
        tokens (str/split input #"\s+")
        verbose? (some #{"-v" "--verbose"} tokens)
        tokens (vec (remove #{"-v" "--verbose"} tokens))
        [precision tokens] (parse-precision tokens)
        input (str/trim (str/join " " tokens))]
    (if (str/blank? input)
      (do
        (binding [*out* *err*]
          (println (usage)))
        (System/exit 1))
      (try
        (let [{:keys [error result from target]} (process-request-text input precision)]
          (if error
            (do
              (binding [*out* *err*]
                (println error))
              (System/exit 1))
            (if verbose?
              (println (str from " = " result " " target))
              (println result))))
        (catch Exception e
          (binding [*out* *err*]
            (println "Error:" (.getMessage e)))
          (System/exit 1))))))
