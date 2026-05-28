(ns unitz.parser
  (:require [clojure.string :as str]))

(def unit-aliases
  {"ft" :ft
   "foot" :ft
   "feet" :ft

   "yd" :yd
   "yard" :yd
   "yards" :yd

   "in" :in
   "inch" :in
   "inches" :in

   "mi" :mi
   "mile" :mi
   "miles" :mi

   "m" :m
   "meter" :m
   "meters" :m

   "km" :km
   "kilometer" :km
   "kilometers" :km

   "cm" :cm
   "centimeter" :cm
   "centimeters" :cm

   "gal" :gal
   "gallon" :gal
   "gallons" :gal

   "l" :l
   "liter" :l
   "liters" :l
   "litre" :l
   "litres" :l

   "ml" :ml
   "milliliter" :ml
   "milliliters" :ml
   "millilitre" :ml
   "millilitres" :ml

   "lb" :lb
   "lbs" :lb
   "pound" :lb
   "pounds" :lb

   "oz" :oz
   "ounce" :oz
   "ounces" :oz

   "g" :g
   "gram" :g
   "grams" :g

   "kg" :kg
   "kilogram" :kg
   "kilograms" :kg

   "h" :hr
   "hr" :hr
   "hrs" :hr
   "hour" :hr
   "hours" :hr

   "min" :min
   "minute" :min
   "minutes" :min

   "s" :s
   "sec" :s
   "second" :s
   "seconds" :s

   "day" :day
   "days" :day

   "week" :week
   "weeks" :week
   "wk" :week

   "yr" :yr
   "year" :yr
   "years" :yr

   "century" :century
   "centuries" :century

   "millennium" :millennium
   "millennia" :millennium
   "millenium" :millennium
   "millenia" :millennium
   "milennium" :millennium
   "milennia" :millennium

   "cup" :cup
   "cups" :cup

   "tbsp" :tbsp
   "tablespoon" :tbsp
   "tablespoons" :tbsp

   "tsp" :tsp
   "teaspoon" :tsp
   "teaspoons" :tsp

   "acre" :acre
   "acres" :acre

   "f" :degF
   "fahrenheit" :degF

   "c" :degC
   "celsius" :degC

   "k" :K
   "kelvin" :K

   "n" :N
   "newton" :N
   "newtons" :N

   "j" :J
   "joule" :J
   "joules" :J

   "w" :W
   "watt" :W
   "watts" :W

   "pa" :Pa
   "pascal" :Pa
   "pascals" :Pa

   "psi" :psi

   ;; Bits
   "bit" :bit
   "bits" :bit

   ;; Bytes (decimal / SI)
   "B" :B
   "byte" :B
   "bytes" :B

   "KB" :KB
   "kb" :KB
   "kilobyte" :KB
   "kilobytes" :KB

   "MB" :MB
   "mb" :MB
   "megabyte" :MB
   "megabytes" :MB

   "GB" :GB
   "gb" :GB
   "gigabyte" :GB
   "gigabytes" :GB

   "TB" :TB
   "tb" :TB
   "terabyte" :TB
   "terabytes" :TB
   "terrabyte" :TB
   "terrabytes" :TB

   "PB" :PB
   "pb" :PB
   "petabyte" :PB
   "petabytes" :PB

   "EB" :EB
   "eb" :EB
   "exabyte" :EB
   "exabytes" :EB

   ;; Bytes (binary / IEC)
   "KiB" :KiB
   "kib" :KiB
   "kibibyte" :KiB
   "kibibytes" :KiB

   "MiB" :MiB
   "mib" :MiB
   "mebibyte" :MiB
   "mebibytes" :MiB

   "GiB" :GiB
   "gib" :GiB
   "gibibyte" :GiB
   "gibibytes" :GiB

   "TiB" :TiB
   "tib" :TiB
   "tebibyte" :TiB
   "tebibytes" :TiB

   "PiB" :PiB
   "pib" :PiB
   "pebibyte" :PiB
   "pebibytes" :PiB

   "EiB" :EiB
   "eib" :EiB
   "exbibyte" :EiB
   "exbibytes" :EiB

   ;; Bits (decimal)
   "Kb" :Kb
   "kilobit" :Kb
   "kilobits" :Kb

   "Mb" :Mb
   "megabit" :Mb
   "megabits" :Mb

   "Gb" :Gb
   "gigabit" :Gb
   "gigabits" :Gb

   "Tb" :Tb
   "terabit" :Tb
   "terabits" :Tb
   "terrabit" :Tb
   "terrabits" :Tb

   "Pb" :Pb
   "petabit" :Pb
   "petabits" :Pb

   "Eb" :Eb
   "exabit" :Eb
   "exabits" :Eb})

(def special-unit-forms
  {"mph" {:mi 1 :hr -1}
   "kph" {:km 1 :hr -1}
   "km/h" {:km 1 :hr -1}
   "kmph" {:km 1 :hr -1}
   "mps" {:m 1 :s -1}
   "m/s" {:m 1 :s -1}
   "fps" {:ft 1 :s -1}
   "ft/s" {:ft 1 :s -1}

   ;; Bit rates (lowercase b = bits)
   "bps"  {:bit 1 :s -1}
   "Kbps" {:Kb 1 :s -1}
   "kbps" {:Kb 1 :s -1}
   "Mbps" {:Mb 1 :s -1}
   "mbps" {:Mb 1 :s -1}
   "Gbps" {:Gb 1 :s -1}
   "gbps" {:Gb 1 :s -1}
   "Tbps" {:Tb 1 :s -1}
   "tbps" {:Tb 1 :s -1}
   "Pbps" {:Pb 1 :s -1}
   "pbps" {:Pb 1 :s -1}
   "Ebps" {:Eb 1 :s -1}
   "ebps" {:Eb 1 :s -1}

   ;; Byte rates (uppercase B = bytes)
   "KBps" {:KB 1 :s -1}
   "MBps" {:MB 1 :s -1}
   "GBps" {:GB 1 :s -1}
   "TBps" {:TB 1 :s -1}
   "PBps" {:PB 1 :s -1}
   "EBps" {:EB 1 :s -1}})

(defn clean-phrase [s]
  (-> s
      str/trim
      (str/replace #"[?]" "")
      (str/replace #"," "")
      (str/replace #"~\s*" "~ ")
      ;; 12ft -> 12 ft, 100kg -> 100 kg
      (str/replace #"(\d)([A-Za-z])" "$1 $2")
      (str/replace #"\s+" " ")
      str/trim))

(defn parse-integer [s]
  #?(:clj (bigint s)
     :cljs (js/parseInt s 10)))

(defn parse-ratio-token [s]
  (let [[n d] (str/split s #"/")]
    (/ (parse-integer n)
       (parse-integer d))))

(defn parse-decimal-token [s]
  #?(:clj (bigdec s)
     :cljs (js/parseFloat s)))

(def number-words
  {"zero" 0 "one" 1 "two" 2 "three" 3 "four" 4 "five" 5
   "six" 6 "seven" 7 "eight" 8 "nine" 9 "ten" 10
   "eleven" 11 "twelve" 12 "thirteen" 13 "fourteen" 14 "fifteen" 15
   "sixteen" 16 "seventeen" 17 "eighteen" 18 "nineteen" 19
   "twenty" 20 "thirty" 30 "forty" 40 "fifty" 50
   "sixty" 60 "seventy" 70 "eighty" 80 "ninety" 90})

(def multiplier-words
  {"hundred" 100 "thousand" 1000 "million" 1000000 "billion" 1000000000})

(defn parse-number-words
  "Parse English number words from tokens starting at index i.
   Returns [value next-index] or nil."
  [tokens i]
  (loop [j i
         total 0
         current 0
         found? false]
    (let [t (some-> (nth tokens j nil) str/lower-case)
          word-val (get number-words t)
          mult-val (get multiplier-words t)]
      (cond
        ;; Skip "and" between number words (e.g. "one hundred and two")
        (and (= "and" t) found?)
        (recur (inc j) total current true)

        ;; A base number word: accumulate into current group
        word-val
        (recur (inc j) total (+ current word-val) true)

        ;; A multiplier: multiply current group and add to total
        (and mult-val found?)
        (let [group (if (zero? current) 1 current)]
          (if (>= mult-val 1000)
            (recur (inc j) (+ total (* group mult-val)) 0 true)
            (recur (inc j) total (* group mult-val) true)))

        ;; Done parsing number words
        found?
        [(+ total current) j]

        :else nil))))

(defn numeric-token? [s]
  (boolean
   (or (re-matches #"\d+" s)
       (re-matches #"\d+\.\d+" s)
       (re-matches #"\d+/\d+" s))))

(defn parse-number-token [s]
  (cond
    (re-matches #"\d+/\d+" s)
    (parse-ratio-token s)

    (re-matches #"\d+\.\d+" s)
    (parse-decimal-token s)

    (re-matches #"\d+" s)
    (parse-integer s)

    :else
    nil))

;; ---------------------------------------------------------------------------
;; Simple arithmetic expression evaluator  (+, -, *, /, parens)
;; ---------------------------------------------------------------------------

(declare ^:private math-parse-expr)

(defn- char-at
  "Get the character at index i of string s as a single-char string."
  [s i]
  (subs s i (inc i)))

(defn- digit? [ch]
  (boolean (re-matches #"\d" ch)))

(defn- whitespace? [ch]
  (boolean (re-matches #"\s" ch)))

(defn- math-tokenize
  "Tokenize a math expression into [:num v], [:op ch], [:lp], [:rp]."
  [s]
  (let [n (count s)]
    (loop [i 0, tokens []]
      (if (>= i n)
        tokens
        (let [ch (char-at s i)]
          (cond
            (whitespace? ch)
            (recur (inc i) tokens)

            (or (digit? ch) (= ch "."))
            (let [end (loop [j (inc i)]
                        (if (and (< j n)
                                 (let [c (char-at s j)]
                                   (or (digit? c) (= c "."))))
                          (recur (inc j))
                          j))
                  ns (subs s i end)
                  v  (if (str/includes? ns ".")
                       (parse-decimal-token ns)
                       (parse-integer ns))]
              (recur end (conj tokens [:num v])))

            (= ch "(") (recur (inc i) (conj tokens [:lp]))
            (= ch ")") (recur (inc i) (conj tokens [:rp]))

            (#{"+" "*" "/" "^"} ch)
            (recur (inc i) (conj tokens [:op ch]))

            (= ch "-")
            (if (or (empty? tokens) (#{:lp :op} (first (peek tokens))))
              ;; Unary minus: absorb into next number
              (let [j   (inc i)
                    end (loop [k j]
                          (if (and (< k n)
                                   (let [c (char-at s k)]
                                     (or (digit? c) (= c "."))))
                            (recur (inc k))
                            k))]
                (if (> end j)
                  (let [ns (subs s i end)
                        v  (if (str/includes? ns ".")
                             (parse-decimal-token ns)
                             (parse-integer ns))]
                    (recur end (conj tokens [:num v])))
                  nil))
              (recur (inc i) (conj tokens [:op ch])))

            :else nil))))))

(defn- math-parse-factor [tokens pos]
  (when (< pos (count tokens))
    (case (first (nth tokens pos))
      :num [(second (nth tokens pos)) (inc pos)]
      :lp  (when-let [[v npos] (math-parse-expr tokens (inc pos))]
             (when (and (< npos (count tokens))
                        (= :rp (first (nth tokens npos))))
               [v (inc npos)]))
      nil)))

(defn- math-pow [base exp]
  #?(:clj
     (cond
       (zero? exp) 1N
       (pos? exp)  (reduce * 1N (repeat exp base))
       :else       (/ 1N (math-pow base (- exp))))
     :cljs
     (js/Math.pow base exp)))

(defn- math-parse-power
  "Parse factor (^ factor)* — right-associative."
  [tokens pos]
  (when-let [[base p0] (math-parse-factor tokens pos)]
    (if (and (< p0 (count tokens))
             (= [:op "^"] (nth tokens p0)))
      (when-let [[exp p1] (math-parse-power tokens (inc p0))]
        [(math-pow base exp) p1])
      [base p0])))

(defn- math-parse-term [tokens pos]
  (when-let [[v0 p0] (math-parse-power tokens pos)]
    (loop [acc v0, p p0]
      (if (and (< p (count tokens))
               (= :op (first (nth tokens p)))
               (#{"*" "/"} (second (nth tokens p))))
        (let [op (second (nth tokens p))]
          (if-let [[v2 p2] (math-parse-power tokens (inc p))]
            (recur (if (= "*" op) (* acc v2) (/ acc v2)) p2)
            [acc p]))
        [acc p]))))

(defn- math-parse-expr [tokens pos]
  (when-let [[v0 p0] (math-parse-term tokens pos)]
    (loop [acc v0, p p0]
      (if (and (< p (count tokens))
               (= :op (first (nth tokens p)))
               (#{"+" "-"} (second (nth tokens p))))
        (let [op (second (nth tokens p))]
          (if-let [[v2 p2] (math-parse-term tokens (inc p))]
            (recur (if (= "+" op) (+ acc v2) (- acc v2)) p2)
            [acc p]))
        [acc p]))))

(defn parse-math
  "Evaluate a simple arithmetic expression string. Returns a number or nil."
  [s]
  (when-let [tokens (math-tokenize (str/trim s))]
    (when (seq tokens)
      (let [[v pos] (math-parse-expr tokens 0)]
        (when (and v (= pos (count tokens)))
          v)))))

(defn evaluate-math-exprs
  "Replace parenthesised arithmetic expressions in `s` with their values."
  [s]
  (let [result (str/replace s #"\(([^()]+)\)"
                            (fn [[match inner]]
                              (if-let [v (parse-math inner)]
                                (str v)
                                match)))]
    (if (= result s)
      s
      (recur result))))

(defn- try-parse-math-tokens
  "Greedily collect num op num op num ... from `tokens` starting at `i`.
   Operators are +, -, * (not / to avoid ambiguity with unit division).
   Returns [value next-index] when at least one operator was consumed, else nil."
  [tokens i]
  (loop [j i, parts []]
    (let [tok (nth tokens j nil)]
      (if (even? (count parts))
        ;; Expecting a number
        (if (and tok (numeric-token? tok))
          (recur (inc j) (conj parts tok))
          (when (>= (count parts) 3)
            (when-let [v (parse-math (str/join " " parts))]
              [v j])))
        ;; Expecting an operator (+, -, *)
        (if (and tok (= 1 (count tok)) (#{"+" "-" "*" "^"} tok))
          (recur (inc j) (conj parts tok))
          (when (>= (count parts) 3)
            (when-let [v (parse-math (str/join " " parts))]
              [v j])))))))

(defn parse-number-at [tokens i]
  (let [t (some-> (nth tokens i nil) str/lower-case)
        t2 (some-> (nth tokens (inc i) nil) str/lower-case)]
    (cond
      (#{"a" "an"} t)
      [1 (inc i)]

      (= "half" t)
      [#?(:clj 1/2 :cljs 0.5) (inc i)]

      (= "quarter" t)
      [#?(:clj 1/4 :cljs 0.25) (inc i)]

      ;; Plain number, possibly followed by mixed fraction or math operators
      (some? (parse-number-token t))
      (or (try-parse-math-tokens tokens i)
          (if (and (some? t2) (re-matches #"\d+/\d+" t2))
            [(+ (parse-number-token t) (parse-number-token t2))
             (+ i 2)]
            [(parse-number-token t) (inc i)]))

      ;; Single token containing math (no spaces): 2+2, 3*4-1
      (and (some? t) (some? (parse-math t)))
      [(parse-math t) (inc i)]

      ;; English number words: "ten", "twenty three", "one hundred", etc.
      (parse-number-words tokens i)
      (parse-number-words tokens i)

      :else
      nil)))

(defn normalize-unit-token [s]
  (let [raw (-> s
                str/trim
                (str/replace #"^[^\w]+|[^\w]+$" ""))
        k   (str/lower-case raw)]
    (or (get unit-aliases raw)
        (get unit-aliases k)
        (throw (ex-info "Unknown unit"
                        {:error :unknown-unit
                         :unit s})))))

(defn unit-map [u exp]
  (cond
    (keyword? u) {u exp}
    (map? u) (into {} (map (fn [[k v]] [k (* exp v)]) u))
    :else (throw (ex-info "Bad unit" {:unit u}))))

(defn merge-unit-maps [& maps]
  (->> maps
       (apply merge-with +)
       (remove (fn [[_ v]] (zero? v)))
       (into {})))

(defn- parse-int-str [s]
  #?(:clj (Integer/parseInt s)
     :cljs (js/parseInt s 10)))

(defn parse-component-token [token]
  (let [raw token
        lower (str/lower-case raw)]
    (cond
      (contains? special-unit-forms raw)
      (get special-unit-forms raw)

      (contains? special-unit-forms lower)
      (get special-unit-forms lower)

      (str/includes? raw "/")
      (let [[num den] (str/split raw #"/" 2)]
        (merge-unit-maps
         (unit-map (parse-component-token num) 1)
         (unit-map (parse-component-token den) -1)))

      (re-matches #"(?i).+\^-?\d+" raw)
      (let [[base exp] (str/split raw #"\^")]
        (unit-map (normalize-unit-token base)
                  (parse-int-str exp)))

      :else
      (normalize-unit-token raw))))

(defn simple-unit-result [components]
  (if (= 1 (count components))
    (first components)
    (apply merge-unit-maps (map #(unit-map % 1) components))))

(defn parse-unit-product [tokens]
  (let [tokens (remove #(#{"a" "an"} (str/lower-case %)) tokens)
        components (map parse-component-token tokens)]
    (simple-unit-result components)))

(defn- vec-index-of [v val]
  (loop [i 0]
    (cond
      (>= i (count v)) -1
      (= (nth v i) val) i
      :else (recur (inc i)))))

(defn parse-unit-phrase [s]
  (let [tokens (->> (str/split (str/trim s) #"\s+")
                    (remove str/blank?)
                    vec)
        lower-tokens (mapv str/lower-case tokens)]
    (cond
      (empty? tokens)
      (throw (ex-info "Missing unit" {:error :missing-unit}))

      (#{"square"} (first lower-tokens))
      (unit-map (parse-unit-phrase (str/join " " (rest tokens))) 2)

      (#{"cubic"} (first lower-tokens))
      (unit-map (parse-unit-phrase (str/join " " (rest tokens))) 3)

      (some #{"per"} lower-tokens)
      (let [i (vec-index-of lower-tokens "per")
            num-tokens (subvec tokens 0 i)
            den-tokens (subvec tokens (inc i))]
        (merge-unit-maps
         (unit-map (parse-unit-product num-tokens) 1)
         (unit-map (parse-unit-product den-tokens) -1)))

      (some #{"/"} lower-tokens)
      (let [i (vec-index-of lower-tokens "/")
            num-tokens (subvec tokens 0 i)
            den-tokens (subvec tokens (inc i))]
        (merge-unit-maps
         (unit-map (parse-unit-product num-tokens) 1)
         (unit-map (parse-unit-product den-tokens) -1)))

      :else
      (parse-unit-product tokens))))

(defn next-number-index [tokens start]
  (loop [i start]
    (cond
      (>= i (count tokens)) nil
      (parse-number-at tokens i) i
      :else (recur (inc i)))))

(defn- split-on-qty-ops
  "Split tokens on * or / that appear between quantity groups.
   A token is a quantity operator when it is followed by something that
   starts a number.  Returns {:segments [[tok ...] ...] :ops [:* or :/ ...]}
   or nil when no quantity-level operators are found."
  [tokens]
  (loop [i 0, current [], segments [], ops []]
    (cond
      (>= i (count tokens))
      (when (seq ops)
        {:segments (conj segments current) :ops ops})

      (and (#{"*" "/"} (nth tokens i))
           (seq current)
           (< (inc i) (count tokens))
           (some? (parse-number-at tokens (inc i))))
      (recur (inc i) [] (conj segments current)
             (conj ops (if (= "*" (nth tokens i)) :* :/)))

      :else
      (recur (inc i) (conj current (nth tokens i)) segments ops))))

(defn- parse-qty-segment
  "Parse a single quantity segment (number + optional unit).
   Returns {:value N :unit U} or nil."
  [seg-tokens]
  (when-let [[value j] (parse-number-at seg-tokens 0)]
    (let [j (if (#{"a" "an"} (some-> (nth seg-tokens j nil) str/lower-case))
              (inc j)
              j)]
      (if (>= j (count seg-tokens))
        {:value value :unit {}}
        {:value value :unit (parse-unit-phrase
                             (str/join " " (subvec seg-tokens j)))}))))

(defn parse-quantity [s]
  (let [tokens (->> (str/split (str/trim s) #"\s+")
                    (remove str/blank?)
                    vec)]
    ;; Try quantity arithmetic: "100 MB / 100 Mbps", "60 mph * 2 hours"
    (or (when-let [{:keys [segments ops]} (split-on-qty-ops tokens)]
          (let [parsed (mapv parse-qty-segment segments)]
            (when (and (every? some? parsed)
                       ;; First operand must have a unit — otherwise it is
                       ;; plain scalar math (e.g. "3 * 4 feet", "100 / 4 feet")
                       (not= {} (:unit (first parsed))))
              {:qty-expr true :terms parsed :ops ops})))
        ;; Existing: simple or mixed quantities
        (loop [i 0
               terms []]
          (if (>= i (count tokens))
            (cond
              (empty? terms)
              (throw (ex-info "Unparseable quantity"
                              {:error :unparseable-quantity
                               :quantity s}))

              (= 1 (count terms))
              (first terms)

              :else
              terms)

            (let [[value j] (or (parse-number-at tokens i)
                                (throw (ex-info "Expected number"
                                                {:error :expected-number
                                                 :token (nth tokens i nil)})))
                  ;; Allows "half a gallon"
                  j (if (#{"a" "an"} (some-> (nth tokens j nil) str/lower-case))
                      (inc j)
                      j)
                  next-i (next-number-index tokens j)
                  unit-tokens (subvec tokens j (or next-i (count tokens)))
                  unit-str (str/join " " unit-tokens)]
              (recur (or next-i (count tokens))
                     (conj terms {:value value
                                  :unit (parse-unit-phrase unit-str)}))))))))

(defn- parse-long-str [s]
  #?(:clj (Long/parseLong s)
     :cljs (js/parseInt s 10)))

(defn extract-format [s]
  (cond
    (re-find #"(?i)\s+rounded to \d+ decimals?$" s)
    (let [[_ n] (re-find #"(?i)\s+rounded to (\d+) decimals?$" s)]
      [(str/replace s #"(?i)\s+rounded to \d+ decimals?$" "")
       {:round (parse-long-str n)}])

    (re-find #"(?i)\s+with \d+ sig figs$" s)
    (let [[_ n] (re-find #"(?i)\s+with (\d+) sig figs$" s)]
      [(str/replace s #"(?i)\s+with \d+ sig figs$" "")
       {:sig-figs (parse-long-str n)}])

    (re-find #"(?i)\s+as a fraction$" s)
    [(str/replace s #"(?i)\s+as a fraction$" "")
     {:style :fraction}]

    :else
    [s nil]))

(defn extract-approx [s]
  (let [lower (str/lower-case s)]
    (cond
      (str/starts-with? lower "about ")
      [(subs s 6) true]

      (str/starts-with? lower "roughly ")
      [(subs s 8) true]

      (str/starts-with? lower "~ ")
      [(subs s 2) true]

      :else
      [s false])))

(defn split-request [s]
  (cond
    ;; how many inches are in 3 feet
    (re-matches #"(?i)^how many .+ are in .+$" s)
    (let [[_ to quantity] (re-matches #"(?i)^how many (.+) are in (.+)$" s)]
      [quantity to])

    ;; how many yards is 12 feet
    (re-matches #"(?i)^how many .+ is .+$" s)
    (let [[_ to quantity] (re-matches #"(?i)^how many (.+) is (.+)$" s)]
      [quantity to])

    ;; 12 feet is how many yards
    (re-matches #"(?i)^.+ is how many .+$" s)
    (let [[_ quantity to] (re-matches #"(?i)^(.+) is how many (.+)$" s)]
      [quantity to])

    ;; how much is 5 kg in pounds
    (re-matches #"(?i)^how much is .+ (in|to) .+$" s)
    (let [[_ quantity _ to] (re-matches #"(?i)^how much is (.+) (in|to) (.+)$" s)]
      [quantity to])

    ;; what is 12 ft in yards
    (re-matches #"(?i)^what is .+ (in|to) .+$" s)
    (let [[_ quantity _ to] (re-matches #"(?i)^what is (.+) (in|to) (.+)$" s)]
      [quantity to])

    ;; convert 12 feet to yards
    (re-matches #"(?i)^convert .+ (in|to) .+$" s)
    (let [[_ quantity _ to] (re-matches #"(?i)^convert (.+) (in|to) (.+)$" s)]
      [quantity to])

    ;; 12 ft in yards / 5 miles to km
    (re-matches #"(?i)^.+ (in|to) .+$" s)
    (let [[_ quantity _ to] (re-matches #"(?i)^(.+) (in|to) (.+)$" s)]
      [quantity to])

    :else
    nil))

(defn parse-error [phrase ex]
  (let [data (ex-data ex)]
    (case (:error data)
      :unknown-unit
      {:error :unknown-unit
       :unit (:unit data)
       :phrase phrase}

      :ambiguous-quantities
      {:error :ambiguous-quantities
       :phrase phrase}

      (throw ex))))

(defn- try-parse-quantity [s]
  (try
    (parse-quantity s)
    true
    #?(:clj (catch Exception _ false)
       :cljs (catch :default _ false))))

(defn- try-swap-sides [quantity-str to-str]
  (try
    (parse-quantity quantity-str)
    [quantity-str to-str]
    #?(:clj
       (catch Exception _
         (try
           (parse-quantity to-str)
           [to-str quantity-str]
           (catch Exception _
             [quantity-str to-str])))
       :cljs
       (catch :default _
         (try
           (parse-quantity to-str)
           [to-str quantity-str]
           (catch :default _
             [quantity-str to-str]))))))

(defn parse-request [phrase]
  (let [original phrase]
    (try
      (let [cleaned (evaluate-math-exprs (clean-phrase phrase))
            [without-format format] (extract-format cleaned)
            [without-approx approx?] (extract-approx without-format)
            pieces (split-request without-approx)]
        (if-not pieces
          {:error :unparseable
           :phrase original}
          (let [[quantity-str to-str] pieces
                ;; Try swapping if quantity side has no number
                ;; e.g. "seconds in one year" -> "one year" to "seconds"
                [quantity-str to-str] (try-swap-sides quantity-str to-str)
                ;; Check if the target side also has a quantity
                to-has-number? (try-parse-quantity to-str)]
            (if to-has-number?
              (throw (ex-info "Both sides have quantities"
                              {:error :ambiguous-quantities
                               :quantity quantity-str
                               :to to-str}))
              (let [request (cond-> {:op :convert
                                     :quantity (parse-quantity quantity-str)
                                     :to (parse-unit-phrase to-str)}
                              approx? (assoc :approx? true)
                              format (assoc :format format))]
                request)))))
      #?(:clj (catch clojure.lang.ExceptionInfo ex
                (or (parse-error original ex)
                    {:error :unparseable
                     :phrase original}))
         :cljs (catch ExceptionInfo ex
                 (or (parse-error original ex)
                     {:error :unparseable
                      :phrase original})))
      #?(:clj (catch Exception _
                {:error :unparseable
                 :phrase original})
         :cljs (catch :default _
                 {:error :unparseable
                  :phrase original})))))
