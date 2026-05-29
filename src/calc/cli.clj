(ns calc.cli
  (:require [calc.units :as u]
            [calc.eval :as ev]
            [calc.format :as fmt]
            [clojure.string :as str]
            [calc.parser :as parser])
  (:gen-class))

(def dim-labels u/dim-categories)

(defn reverse-aliases
  "Build a map from canonical unit keyword to sorted list of alias strings."
  []
  (into {}
        (for [[k v] u/unit-defs :when (:aliases v)]
          [k (:aliases v)])))

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

(defn format-error [err]
  (str "Error: " (fmt/format-error err)))

(defn process-request-text [input fmt-opts]
  (let [parsed (parser/parse-request input)
        effective-fmt (merge (:format parsed) fmt-opts)
        result (ev/convert-request parsed)]
    (if-not (:ok? result)
      {:error (format-error result)}
      (if (:unit-label result)
        {:result (str (fmt/format-number (:value result) effective-fmt)
                      (when (:unit-label result)
                        (str " " (:unit-label result))))
         :from input
         :target nil}
        (let [[display-input _] (parser/extract-format input)
              {:keys [from target]} (parser/split-display-parts display-input)]
          {:result (fmt/format-number (:value result) effective-fmt)
           :from from
           :target target})))))

(defn usage []
  (str/join
   "\n"
   ["Usage:"
    "  calc <request>"
    "  calc --list [--kind <kind>]"
    ""
    "Options:"
    "  -n, --numeric          Output bare number only (requires explicit target unit)"
    "  -p, --precision N      Round to N decimal places"
    "  -s, --sig-figs N       Round to N significant figures"
    "  -v, --verbose          Verbose output"
    "  -k, --kind <kind>      Filter --list by unit kind"
    "      --list             List supported units"
    "      --help             Show this help"
    ""
    "Examples:"
    "  calc 12 feet in yards"
    "  calc 2 cubic yards to gallons"
    "  calc -n 100GB / 900Mbps in days"
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
                   (assoc (or opts {}) :round (Long/parseLong (second remaining)))
                   result))

          (and (or (= "-s" t) (= "--sig-figs" t))
               (second remaining))
          (do
            (when (:round opts)
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
          help? (some #{"-h" "--help"} tokens)
          tokens (vec (remove #{"-h" "--help"} tokens))
          numeric? (some #{"-n" "--numeric"} tokens)
          tokens (vec (remove #{"-n" "--numeric"} tokens))
          [fmt-opts tokens] (parse-format-opts tokens)
          input (str/trim (str/join " " tokens))]
      (cond
        help?
        (println (usage))

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
        (let [parsed (parser/parse-request input)]
          (when (and numeric? (= :auto (:to parsed)))
            (binding [*out* *err*]
              (println "Error: --numeric requires an explicit target unit (e.g., \"in days\")"))
            (System/exit 1))
          (if numeric?
            (let [effective-fmt (merge (:format parsed) fmt-opts)
                  result (ev/convert-request parsed)]
              (if-not (:ok? result)
                (do
                  (binding [*out* *err*]
                    (println (format-error result)))
                  (System/exit 1))
                (println (fmt/format-number (:value result) effective-fmt))))
            (let [{:keys [error result from target]} (process-request-text input fmt-opts)]
              (if error
                (do
                  (binding [*out* *err*]
                    (println error))
                  (System/exit 1))
                (if (and from target)
                  (println (str from " = " result " " target))
                  (println result))))))))
    (catch Exception e
      (binding [*out* *err*]
        (println "Error:" (.getMessage e)))
      (System/exit 1))))
