;; src/unitz/cli.clj
(ns unitz.cli
  (:require [unitz.core :as u]
            [clojure.string :as str]
            [unitz.parser :as parser])
  (:gen-class))

(def dim-labels
  {{:length 1} "Length"
   {:length 2} "Area"
   {:length 3} "Volume"
   {:mass 1} "Mass"
   {:time 1} "Time"
   {:data 1} "Data"
   {:mass 1 :length 1 :time -2} "Force"
   {:mass 1 :length 2 :time -2} "Energy"
   {:mass 1 :length 2 :time -3} "Power"
   {:mass 1 :length -1 :time -2} "Pressure"})

(defn reverse-aliases
  "Build a map from canonical unit keyword to sorted list of alias strings."
  []
  (reduce-kv
   (fn [m alias-str unit-kw]
     (update m unit-kw (fnil conj []) alias-str))
   {}
   parser/unit-aliases))

(def label->dim
  (into {} (map (fn [[k v]] [(str/lower-case v) k]) dim-labels)))

(def all-kind-names
  (sort (concat (map (comp str/lower-case val) dim-labels)
                ["temperature" "compound"])))

(defn format-section [label entries aliases]
  (let [lines (for [[unit-kw _] (sort-by (comp str first) entries)
                    :let [names (sort (get aliases unit-kw))]]
                (str "  " (name unit-kw)
                     (when (seq names)
                       (str "  (" (str/join ", " names) ")"))))]
    (str label ":\n" (str/join "\n" lines))))

(defn format-temp-section [aliases]
  (let [temp-units [{:key :degF :aliases (get aliases :degF)}
                    {:key :degC :aliases (get aliases :degC)}
                    {:key :K    :aliases (get aliases :K)}]]
    (str "Temperature:\n"
         (str/join "\n"
                   (for [{:keys [key aliases]} temp-units
                         :let [names (sort aliases)]]
                     (str "  " (name key)
                          (when (seq names)
                            (str "  (" (str/join ", " names) ")"))))))))

(defn format-compound-section []
  (str "Compound:\n"
       (str/join "\n"
                 (for [[alias-str _] (sort parser/special-unit-forms)]
                   (str "  " alias-str)))))

(defn list-units
  ([] (list-units nil))
  ([kind]
   (let [aliases (reverse-aliases)
         by-dim (group-by (fn [[_ v]] (:dim v)) u/unit-defs)
         dim-order [{:length 1} {:length 2} {:length 3}
                    {:mass 1} {:time 1} {:data 1}
                    {:mass 1 :length 1 :time -2}
                    {:mass 1 :length 2 :time -2}
                    {:mass 1 :length 2 :time -3}
                    {:mass 1 :length -1 :time -2}]
         kind-lower (some-> kind str/lower-case)]
     (if kind-lower
       ;; Filtered to a single kind
       (cond
         (= "temperature" kind-lower)
         (format-temp-section aliases)

         (= "compound" kind-lower)
         (format-compound-section)

         (get label->dim kind-lower)
         (let [dim (get label->dim kind-lower)
               entries (get by-dim dim)
               label (get dim-labels dim)]
           (if entries
             (format-section label entries aliases)
             (str "No units found for kind: " kind)))

         :else
         (str "Unknown kind: " kind "\nAvailable: " (str/join ", " all-kind-names)))
       ;; Show all
       (let [sections
             (for [dim dim-order
                   :let [entries (get by-dim dim)]
                   :when entries]
               (format-section (get dim-labels dim (pr-str dim)) entries aliases))]
         (str/join "\n\n" (concat sections
                                  [(format-temp-section aliases)
                                   (format-compound-section)])))))))

(defn format-number
  ([x] (format-number x nil))
  ([x {:keys [precision sig-figs] :as opts}]
   (cond
     precision
     (let [bd (if (instance? java.math.BigDecimal x)
                x
                (BigDecimal. (double x)))]
       (.toPlainString (.setScale bd (int precision) java.math.RoundingMode/HALF_UP)))

     sig-figs
     (let [bd (if (instance? java.math.BigDecimal x)
                x
                (BigDecimal. (double x)))]
       (.toPlainString (.stripTrailingZeros
                        (.round bd (java.math.MathContext. (int sig-figs) java.math.RoundingMode/HALF_UP)))))

     :else
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

(defn process-request-text [input fmt-opts]
  (let [parsed (parser/parse-request input)
        result (u/convert-request parsed)]
    (if (and (map? result) (contains? result :error))
      {:error (format-error result)}
      (let [[lhs rhs] (split-input-parts input)
            has-number? #(re-find #"\d|^(?:a|an|one|two|three|four|five|six|seven|eight|nine|ten|eleven|twelve|thirteen|fourteen|fifteen|sixteen|seventeen|eighteen|nineteen|twenty|thirty|forty|fifty|sixty|seventy|eighty|ninety|hundred|thousand|million|billion|half|quarter)\b" (str/lower-case (or % "")))
            swapped? (and (not (has-number? lhs)) (has-number? rhs))]
        {:result (format-number result fmt-opts)
         :from (if swapped? rhs lhs)
         :target (if swapped? lhs rhs)}))))

(defn usage []
  (str/join
   "\n"
   ["Usage:"
    "  unitz <request>"
    "  unitz --list [--kind <kind>]"
    ""
    "Examples:"
    "  unitz 12 feet in yards"
    "  unitz 2 cubic yards to gallons"
    "  unitz --list"
    "  unitz --list --kind length"]))

(defn parse-format-opts [tokens]
  (loop [remaining tokens
         opts nil
         result []]
    (if (empty? remaining)
      [opts result]
      (let [t (first remaining)]
        (cond
          (and (or (= "-p" t) (= "--precision" t))
               (second remaining))
          (do
            (when (:sig-figs opts)
              (throw (ex-info "Cannot use both -p and -s" {})))
            (recur (drop 2 remaining)
                   (assoc (or opts {}) :precision (Long/parseLong (second remaining)))
                   result))

          (and (or (= "-s" t) (= "--sig-figs" t))
               (second remaining))
          (do
            (when (:precision opts)
              (throw (ex-info "Cannot use both -p and -s" {})))
            (recur (drop 2 remaining)
                   (assoc (or opts {}) :sig-figs (Long/parseLong (second remaining)))
                   result))

          :else
          (recur (rest remaining) opts (conj result t)))))))

(defn extract-flag
  "Remove a flag and its argument from tokens. Returns [value remaining-tokens]."
  [tokens flag-short flag-long]
  (loop [remaining tokens
         value nil
         result []]
    (if (empty? remaining)
      [value result]
      (let [t (first remaining)]
        (if (and (or (= flag-short t) (= flag-long t))
                 (second remaining))
          (recur (drop 2 remaining) (second remaining) result)
          (recur (rest remaining) value (conj result t)))))))

(defn -main [& args]
  (try
    (let [input (str/trim (str/join " " args))
          tokens (str/split input #"\s+")
          list? (some #{"--list"} tokens)
          tokens (vec (remove #{"--list"} tokens))
          [kind tokens] (extract-flag tokens "-k" "--kind")
          verbose? (some #{"-v" "--verbose"} tokens)
          tokens (vec (remove #{"-v" "--verbose"} tokens))
          [fmt-opts tokens] (parse-format-opts tokens)
          input (str/trim (str/join " " tokens))]
      (cond
        list?
        (println (list-units kind))

        (and kind (not list?))
        (println (list-units kind))

        (str/blank? input)
        (do
          (binding [*out* *err*]
            (println (usage)))
          (System/exit 1))

        :else
        (let [{:keys [error result from target]} (process-request-text input fmt-opts)]
          (if error
            (do
              (binding [*out* *err*]
                (println error))
              (System/exit 1))
            (if verbose?
              (println (str from " = " result " " target))
              (println result))))))
    (catch Exception e
      (binding [*out* *err*]
        (println "Error:" (.getMessage e)))
      (System/exit 1))))
