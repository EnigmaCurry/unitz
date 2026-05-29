(ns calc.cli
  (:require [calc.core :as u]
            [clojure.string :as str]
            [calc.parser :as parser])
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
   {:mass 1 :length -1 :time -2} "Pressure"
   {:mass 1 :length 2 :time -3 :current -1} "Electrical Potential"
   {:current 1} "Electric Current"
   {:mass 1 :length 2 :time -3 :current -2} "Resistance"
   {:current 2 :time 4 :mass -1 :length -2} "Capacitance"
   {:mass 1 :length 2 :time -2 :current -2} "Inductance"})

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

(def compound-categories
  [["Velocity"   #{:mi :km :m :ft} #{:hr :s}]
   ["Bitrate"    #{:bit :Kb :Mb :Gb :Tb :Pb :Eb} #{:s}]
   ["Byte Rate"  #{:KB :MB :GB :TB :PB :EB} #{:s}]])

(defn- classify-compound [[_ unit-map]]
  (let [pos-keys (set (keys (into {} (filter (fn [[_ v]] (pos? v)) unit-map))))
        neg-keys (set (keys (into {} (filter (fn [[_ v]] (neg? v)) unit-map))))]
    (or (some (fn [[label num-set den-set]]
                (when (and (every? num-set pos-keys)
                           (every? den-set neg-keys))
                  label))
              compound-categories)
        "Other")))

(defn format-compound-section []
  (let [grouped (group-by classify-compound (sort parser/special-unit-forms))
        order (concat (map first compound-categories) ["Other"])]
    (str/join "\n\n"
              (for [label order
                    :let [entries (get grouped label)]
                    :when (seq entries)]
                (str label ":\n"
                     (str/join "\n"
                               (for [[alias-str _] (sort entries)]
                                 (str "  " alias-str))))))))

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
                    {:mass 1 :length -1 :time -2}
                    {:mass 1 :length 2 :time -3 :current -1}
                    {:current 1}
                    {:mass 1 :length 2 :time -3 :current -2}
                    {:current 2 :time 4 :mass -1 :length -2}
                    {:mass 1 :length 2 :time -2 :current -2}]
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

(defn split-input-parts
  "Extract [from target] from the input, where from is the quantity side
   and target is the unit-only side."
  [input]
  (or
   ;; how many X are in Y → from=Y, target=X
   (when-let [[_ to from] (re-matches #"(?i)^how many (.+) are in (.+)$" input)]
     [(str/trim from) (str/trim to)])
   ;; how many X is Y → from=Y, target=X
   (when-let [[_ to from] (re-matches #"(?i)^how many (.+) is (.+)$" input)]
     [(str/trim from) (str/trim to)])
   ;; Y is how many X → from=Y, target=X (but parser may swap)
   (when-let [[_ from to] (re-matches #"(?i)^(.+) is how many (.+)$" input)]
     [(str/trim from) (str/trim to)])
   ;; Generic "X in/to Y"
   (when-let [[_ lhs _ rhs] (re-matches #"(?i)^(?:(?:how much is|what is|convert)\s+)?(.+?)\s+(in|to)\s+(.+?)$" input)]
     [(str/trim lhs) (str/trim rhs)])))

(defn process-request-text [input fmt-opts]
  (let [parsed (parser/parse-request input)
        result (u/convert-request parsed)]
    (cond
      (and (map? result) (contains? result :error))
      {:error (format-error result)}

      ;; Auto-scaled result (no target unit specified)
      (and (map? result) (contains? result :unit-label))
      {:result (str (format-number (:value result) fmt-opts)
                    (when (:unit-label result)
                      (str " " (:unit-label result))))
       :from input
       :target nil}

      :else
      (let [[from target] (split-input-parts input)]
        {:result (format-number result fmt-opts)
         :from from
         :target target}))))

(defn usage []
  (str/join
   "\n"
   ["Usage:"
    "  calc <request>"
    "  calc --list [--kind <kind>]"
    ""
    "Examples:"
    "  calc 12 feet in yards"
    "  calc 2 cubic yards to gallons"
    "  calc --list"
    "  calc --list --kind length"]))

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
            (if (and from target)
              (println (str from " = " result " " target))
              (println result))))))
    (catch Exception e
      (binding [*out* *err*]
        (println "Error:" (.getMessage e)))
      (System/exit 1))))
