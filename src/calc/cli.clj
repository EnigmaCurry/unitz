(ns calc.cli
  (:require [calc.units :as u]
            [calc.eval :as ev]
            [calc.format :as fmt]
            [clojure.string :as str]
            [calc.parser :as parser])
  (:import (org.jline.reader LineReaderBuilder EndOfFileException UserInterruptException LineReader)
           (org.jline.terminal TerminalBuilder))
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

(defn- format-unit-label
  "Format a unit keyword or exponent map as a short canonical string."
  [unit]
  (cond
    (keyword? unit)
    (get u/unit-short-names unit (name unit))

    (map? unit)
    (let [pos (into {} (filter (fn [[_ v]] (pos? v))) unit)
          neg (into {} (filter (fn [[_ v]] (neg? v))) unit)]
      (str (str/join "·" (for [[k v] pos]
                           (let [label (get u/unit-short-names k (name k))]
                             (if (= v 1) label (str label v)))))
           (when (seq neg)
             (str "/" (str/join "·" (for [[k v] neg]
                                     (let [label (get u/unit-short-names k (name k))
                                           exp (- v)]
                                       (if (= exp 1) label (str label exp)))))))))

    :else (str unit)))

(defn- format-quantity-label
  "Build a canonical display string like '2 m/s' from a parsed quantity."
  [quantity]
  (cond
    (map? quantity)
    (let [val-str (fmt/format-number (:value quantity) nil)]
      (str val-str " " (format-unit-label (:unit quantity))))

    (vector? quantity)
    (str/join " " (map format-quantity-label quantity))

    :else (str quantity)))

(defn process-request-text [input fmt-opts]
  (if-let [math-result (parser/parse-math input)]
    {:result (fmt/format-number math-result (assoc (or fmt-opts {}) :original-expr input))}
    (let [parsed (parser/parse-request input)
          effective-fmt (merge (:format parsed) fmt-opts)
          result (ev/convert-request parsed)]
      (if-not (:ok? result)
        {:error (format-error result)}
        (if (:unit-label result)
          {:result (str (fmt/format-number (:value result) effective-fmt)
                        " " (:unit-label result))
           :from (format-quantity-label (:quantity parsed))
           :target nil}
          (if (= :auto (:to parsed))
            ;; Auto-scale couldn't find a better unit — display original quantity as-is
            {:result (format-quantity-label (:quantity parsed))
             :from nil
             :target nil}
            {:result (fmt/format-number (:value result) effective-fmt)
             :from (format-quantity-label (:quantity parsed))
             :target (format-unit-label (:to parsed))}))))))

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

(defn- tty?
  "Return true if stdin appears to be an interactive terminal."
  []
  (let [terminal (-> (TerminalBuilder/builder) (.system true) (.build))
        result (not= "dumb" (.getType terminal))]
    (.close terminal)
    result))

(defn- repl-help []
  (str/join
   "\n"
   ["REPL commands:"
    "  /help             Show this help"
    "  /p N              Set precision to N decimal places"
    "  /s N              Set significant figures to N"
    "  /p                Clear precision setting"
    "  /s                Clear sig-figs setting"
    "  /clear            Clear screen and history"
    "  /reset            Clear screen and history"
    "  /quit, /exit      Exit the REPL"
    ""
    "Type any calculation expression at the prompt."]))

(defn- parse-slash-command
  "Parse a slash command from trimmed input. Returns [cmd arg] or nil if not a slash command."
  [input]
  (when (str/starts-with? input "/")
    (let [parts (str/split (subs input 1) #"\s+" 2)
          cmd (first parts)
          arg (second parts)]
      [cmd arg])))

(def ^:private hist-path (str (System/getProperty "user.home") "/.calc_history"))

(defn- clear-screen-and-history
  "Delete the history file and clear the terminal screen."
  []
  (let [hf (java.io.File. hist-path)]
    (when (.exists hf) (.delete hf)))
  (print "\033[2J\033[H")
  (flush))

(defn repl
  "Launch an interactive REPL with JLine readline support."
  []
  (let [terminal (-> (TerminalBuilder/builder) (.system true) (.build))
        reader   (-> (LineReaderBuilder/builder) (.terminal terminal) (.build))]
    (.setVariable reader LineReader/HISTORY_FILE hist-path)
    (println "calc — type '/help' for usage, Ctrl-D to exit")
    (loop [fmt-opts nil]
      (let [line (try (.readLine reader "calc> ")
                      (catch EndOfFileException _ ::eof)
                      (catch UserInterruptException _ ::interrupt))
            next-opts
            (cond
              (= ::eof line) ::exit
              (= ::interrupt line) fmt-opts
              :else
              (let [input (str/trim line)]
                (cond
                  (str/blank? input) fmt-opts

                  (#{"exit" "quit"} input) ::exit

                  (str/starts-with? input "/")
                  (let [[cmd arg] (parse-slash-command input)]
                    (case cmd
                      "help"
                      (do (println (repl-help)) fmt-opts)

                      ("clear" "reset")
                      (do (clear-screen-and-history) ::restart)

                      "p"
                      (if (str/blank? arg)
                        (do (println "Precision cleared.")
                            (dissoc (or fmt-opts {}) :round))
                        (try
                          (let [n (Long/parseLong (str/trim arg))]
                            (println (str "Precision set to " n " decimal places."))
                            (-> (or fmt-opts {}) (dissoc :sig-figs) (assoc :round n)))
                          (catch NumberFormatException _
                            (println "Error: /p requires a number")
                            fmt-opts)))

                      "s"
                      (if (str/blank? arg)
                        (do (println "Sig-figs cleared.")
                            (dissoc (or fmt-opts {}) :sig-figs))
                        (try
                          (let [n (Long/parseLong (str/trim arg))]
                            (println (str "Sig-figs set to " n "."))
                            (-> (or fmt-opts {}) (dissoc :round) (assoc :sig-figs n)))
                          (catch NumberFormatException _
                            (println "Error: /s requires a number")
                            fmt-opts)))

                      ("quit" "exit")
                      ::exit

                      ;; unknown slash command
                      (do (println (str "Unknown command: /" cmd))
                          fmt-opts)))

                  (= "help" input)
                  (do (println (repl-help)) fmt-opts)

                  :else
                  (do
                    (try
                      (let [{:keys [error result from target]} (process-request-text input fmt-opts)]
                        (if error
                          (println error)
                          (if (and from target)
                            (println (str from " = " result " " target))
                            (println result))))
                      (catch Exception e
                        (println "Error:" (.getMessage e))))
                    fmt-opts))))]
        (cond
          (= ::exit next-opts) (.close terminal)
          (= ::restart next-opts) (do (.close terminal) (repl))
          :else (recur next-opts))))))

(defn process-stdin
  "Read lines from stdin and process each as a calc request."
  []
  (let [has-error (volatile! false)]
    (doseq [line (line-seq (java.io.BufferedReader. (java.io.InputStreamReader. System/in)))
            :let [input (str/trim line)]
            :when (not (str/blank? input))]
      (let [{:keys [error result from target]} (process-request-text input nil)]
        (if error
          (do (vreset! has-error true)
              (binding [*out* *err*] (println error)))
          (if (and from target)
            (println (str from " = " result " " target))
            (println result)))))
    (when @has-error
      (System/exit 1))))

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
        (or help? (= "help" input))
        (println (usage))

        list?
        (println (list-units kind))

        (and kind (not list?))
        (println (list-units kind))

        (str/blank? input)
        (if (tty?)
          (repl)
          (process-stdin))

        :else
        (if-let [math-result (parser/parse-math input)]
          (println (fmt/format-number math-result (assoc (if numeric? (assoc (or fmt-opts {}) :numeric true) (or fmt-opts {}))
                                                         :original-expr input)))
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
                    (println result)))))))))
    (catch Exception e
      (binding [*out* *err*]
        (println "Error:" (.getMessage e)))
      (System/exit 1))))
