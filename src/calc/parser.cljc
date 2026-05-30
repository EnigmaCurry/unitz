(ns calc.parser
  (:require [clojure.string :as str]
            [calc.units :as units]))

(def unit-aliases units/unit-aliases)

(def special-unit-forms
  {"mph" {:mi 1 :hr -1}
   "kph" {:km 1 :hr -1}
   "km/h" {:km 1 :hr -1}
   "kmph" {:km 1 :hr -1}
   "mps" {:m 1 :s -1}
   "m/s" {:m 1 :s -1}
   "fps" {:ft 1 :s -1}
   "ft/s" {:ft 1 :s -1}

   ;; Area / volume shorthand
   "sqft" {:ft 2}
   "sqm"  {:m 2}
   "sqyd" {:yd 2}
   "sqmi" {:mi 2}
   "sqkm" {:km 2}
   "sqin" {:in 2}
   "cuft" {:ft 3}
   "cum"  {:m 3}
   "cuyd" {:yd 3}

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
      ;; But preserve ordinals like 4th, 2nd, 3rd, 5th etc.
      ;; And preserve scientific notation like 10E9, 3.5e-12
      (str/replace #"(\d)(?!(?:st|nd|rd|th)\b)(?![eE][+-]?\d)([A-Za-z])" "$1 $2")
      ;; Normalize % between two numbers to mod (modulo), standalone % to percent
      (str/replace #"(\d)\s*%\s*(\d)" "$1 mod $2")
      (str/replace #"(\d)\s*%" "$1 percent")
      ;; Collapse whitespace around / between unit words: "meters / second" → "meters/second"
      (str/replace #"([A-Za-z])\s*/\s*([A-Za-z])" "$1/$2")
      ;; Collapse whitespace around / between digits: "21349 /234234" → "21349/234234"
      (str/replace #"(\d)\s*/\s*(\d)" "$1/$2")
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
       (re-matches #"\d+/\d+" s)
       (re-matches #"(?i)\d+(?:\.\d+)?[eE][+-]?\d+" s))))

(defn parse-sci-token [s]
  #?(:clj (bigdec s)
     :cljs (js/parseFloat s)))

(defn parse-number-token [s]
  (cond
    (re-matches #"\d+/\d+" s)
    (parse-ratio-token s)

    (re-matches #"(?i)\d+(?:\.\d+)?[eE][+-]?\d+" s)
    (parse-sci-token s)

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

(defn- alpha? [ch]
  (boolean (re-matches #"[A-Za-z_]" ch)))

(defn- math-tokenize
  "Tokenize a math expression into [:num v], [:op ch], [:lp], [:rp], [:fn name]."
  [s]
  (let [n (count s)]
    (loop [i 0, tokens []]
      (if (>= i n)
        tokens
        (let [ch (char-at s i)]
          (cond
            (whitespace? ch)
            (recur (inc i) tokens)

            ;; Function names: sqrt, cbrt, root
            (alpha? ch)
            (let [end (loop [j (inc i)]
                        (if (and (< j n) (alpha? (char-at s j)))
                          (recur (inc j))
                          j))
                  word (str/lower-case (subs s i end))]
              (if (#{"sqrt" "cbrt" "root"} word)
                (recur end (conj tokens [:fn word]))
                nil))

            (or (digit? ch) (= ch "."))
            (let [end (loop [j (inc i)]
                        (if (and (< j n)
                                 (let [c (char-at s j)]
                                   (or (digit? c) (= c "."))))
                          (recur (inc j))
                          j))
                  ;; Consume scientific notation suffix: e/E followed by optional +/- and digits
                  end (if (and (< end n)
                              (#{"e" "E"} (char-at s end)))
                        (let [after-e (inc end)
                              after-sign (if (and (< after-e n)
                                                  (#{"+" "-"} (char-at s after-e)))
                                           (inc after-e)
                                           after-e)
                              digit-end (loop [k after-sign]
                                          (if (and (< k n) (digit? (char-at s k)))
                                            (recur (inc k))
                                            k))]
                          (if (> digit-end after-sign)
                            digit-end
                            end))
                        end)
                  ns (subs s i end)
                  v  (if (or (str/includes? ns ".") (re-find #"[eE]" ns))
                       (parse-sci-token ns)
                       (parse-integer ns))]
              (recur end (conj tokens [:num v])))

            (= ch "(") (recur (inc i) (conj tokens [:lp]))
            (= ch ")") (recur (inc i) (conj tokens [:rp]))

            (= ch ",") (recur (inc i) (conj tokens [:comma]))

            (#{"+" "*" "/" "^" "%"} ch)
            (recur (inc i) (conj tokens [:op ch]))

            (= ch "-")
            (if (or (empty? tokens) (#{:lp :op :comma} (first (peek tokens))))
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

(defn- math-nth-root
  "Compute the nth root of x. Returns exact integer when possible, else decimal."
  [x n]
  #?(:clj
     (let [xd (double x)]
       (cond
         (zero? xd) 0N
         :else
         (let [approx (Math/pow (Math/abs xd) (/ 1.0 (double n)))
               candidate (Math/round approx)]
           (if (and (pos? xd)
                    (pos? candidate)
                    (= (reduce *' (repeat (long n) (bigint candidate)))
                       (bigint x)))
             (bigint candidate)
             (bigdec (Math/pow xd (/ 1.0 (double n))))))))
     :cljs
     (let [result (js/Math.pow x (/ 1.0 n))]
       (if (== result (js/Math.round result))
         (js/Math.round result)
         result))))

(defn- math-parse-factor [tokens pos]
  (when (< pos (count tokens))
    (case (first (nth tokens pos))
      :num [(second (nth tokens pos)) (inc pos)]
      :fn  (let [fname (second (nth tokens pos))
                 npos (inc pos)]
             ;; Expect '(' after function name
             (when (and (< npos (count tokens))
                        (= :lp (first (nth tokens npos))))
               (case fname
                 ;; sqrt(expr)
                 "sqrt"
                 (when-let [[v p1] (math-parse-expr tokens (inc npos))]
                   (when (and (< p1 (count tokens))
                              (= :rp (first (nth tokens p1))))
                     [(math-nth-root v 2) (inc p1)]))

                 ;; cbrt(expr)
                 "cbrt"
                 (when-let [[v p1] (math-parse-expr tokens (inc npos))]
                   (when (and (< p1 (count tokens))
                              (= :rp (first (nth tokens p1))))
                     [(math-nth-root v 3) (inc p1)]))

                 ;; root(n, expr)
                 "root"
                 (when-let [[degree p1] (math-parse-expr tokens (inc npos))]
                   (when (and (< p1 (count tokens))
                              (= :comma (first (nth tokens p1))))
                     (when-let [[v p2] (math-parse-expr tokens (inc p1))]
                       (when (and (< p2 (count tokens))
                                  (= :rp (first (nth tokens p2))))
                         [(math-nth-root v degree) (inc p2)]))))

                 nil)))
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

(defn- math-div [a b]
  #?(:clj  (try (/ a b)
                (catch ArithmeticException _
                  (.divide (bigdec a) (bigdec b) (java.math.MathContext. 34))))
     :cljs (/ a b)))

(defn- math-parse-term [tokens pos]
  (when-let [[v0 p0] (math-parse-power tokens pos)]
    (loop [acc v0, p p0]
      (if (and (< p (count tokens))
               (= :op (first (nth tokens p)))
               (#{"*" "/" "%"} (second (nth tokens p))))
        (let [op (second (nth tokens p))]
          (if-let [[v2 p2] (math-parse-power tokens (inc p))]
            (recur (case op
                     "*" (* acc v2)
                     "/" (math-div acc v2)
                     "%" (mod acc v2))
                   p2)
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
  "Replace parenthesised arithmetic expressions in `s` with their values.
   Skips parentheses immediately preceded by function names (sqrt, cbrt, root)."
  [s]
  (let [result (str/replace s #"(?<![A-Za-z])\(([^()]+)\)"
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
        (if (and tok (= 1 (count tok)) (#{"+" "-" "*" "^" "%"} tok))
          (recur (inc j) (conj parts tok))
          (when (>= (count parts) 3)
            (when-let [v (parse-math (str/join " " parts))]
              [v j])))))))

(def ordinal-fractions
  {"half"       #?(:clj 1/2 :cljs 0.5)
   "third"      #?(:clj 1/3 :cljs (/ 1 3))
   "quarter"    #?(:clj 1/4 :cljs 0.25)
   "fifth"      #?(:clj 1/5 :cljs 0.2)
   "sixth"      #?(:clj 1/6 :cljs (/ 1 6))
   "seventh"    #?(:clj 1/7 :cljs (/ 1 7))
   "eighth"     #?(:clj 1/8 :cljs 0.125)
   "ninth"      #?(:clj 1/9 :cljs (/ 1 9))
   "tenth"      #?(:clj 1/10 :cljs 0.1)
   "sixteenth"  #?(:clj 1/16 :cljs 0.0625)})

(defn parse-number-at [tokens i]
  (let [t (some-> (nth tokens i nil) str/lower-case)
        t2 (some-> (nth tokens (inc i) nil) str/lower-case)]
    (cond
      (and (#{"a" "an"} t) (contains? ordinal-fractions t2))
      [(ordinal-fractions t2) (+ i 2)]

      (#{"a" "an"} t)
      [1 (inc i)]

      (contains? ordinal-fractions t)
      [(ordinal-fractions t) (inc i)]

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

      (str/includes? raw "*")
      (let [parts (str/split raw #"\*")]
        (apply merge-unit-maps
               (map #(unit-map (parse-component-token %) 1) parts)))

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

(def ^:private compound-prefixes
  #{"nautical" "metric" "short" "fluid" "fl"})

(defn- join-compound-tokens
  "Join adjacent tokens where the first is a known compound prefix
   (e.g. 'nautical' 'mile' → 'nautical mile')."
  [tokens]
  (loop [remaining (seq tokens) result []]
    (if-not remaining
      result
      (let [t (first remaining)
            nxt (second remaining)]
        (if (and nxt (compound-prefixes (str/lower-case t)))
          (recur (nnext remaining) (conj result (str t " " nxt)))
          (recur (next remaining) (conj result t)))))))

(defn parse-unit-product [tokens]
  (let [tokens (->> tokens
                    (remove #(#{"a" "an"} (str/lower-case %)))
                    join-compound-tokens)
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

      ;; "time" is shorthand for "hours, minutes, and seconds"
      (= ["time"] lower-tokens)
      [{:hr 1} {:min 1} {:s 1}]

      ;; Mixed output target: "feet and inches", "hours, minutes, and seconds"
      ;; Commas are stripped by clean-phrase, so "hours, minutes, and seconds"
      ;; becomes "hours minutes and seconds". We split on "and" first, then
      ;; expand multi-token groups into individual units (unless they form a
      ;; known compound like "nautical miles").
      (some #{"and"} lower-tokens)
      (let [;; Split on "and" into groups of tokens
            groups (loop [remaining tokens, current [], groups []]
                     (if (empty? remaining)
                       (if (seq current)
                         (conj groups current)
                         groups)
                       (if (= "and" (str/lower-case (first remaining)))
                         (recur (rest remaining)
                                []
                                (if (seq current)
                                  (conj groups current)
                                  groups))
                         (recur (rest remaining)
                                (conj current (first remaining))
                                groups))))
            ;; Expand multi-token groups: try as compound unit first,
            ;; if it produces a multi-dimensional result, split into individual tokens
            units (reduce
                   (fn [acc group]
                     (if (= 1 (count group))
                       (conj acc (first group))
                       ;; Multi-token group — check if it's a compound unit
                       (let [joined (str/join " " group)]
                         (if (try (normalize-unit-token joined) true
                                  #?(:clj (catch Exception _ false)
                                     :cljs (catch :default _ false)))
                           (conj acc joined)
                           (into acc group)))))
                   []
                   groups)]
        (if (>= (count units) 2)
          (mapv #(parse-unit-phrase %) units)
          (parse-unit-product tokens)))

      (#{"square" "sq" "squared"} (first lower-tokens))
      (unit-map (parse-unit-phrase (str/join " " (rest tokens))) 2)

      (#{"cubic" "cu" "cubed"} (first lower-tokens))
      (unit-map (parse-unit-phrase (str/join " " (rest tokens))) 3)

      (#{"square" "squared"} (last lower-tokens))
      (unit-map (parse-unit-phrase (str/join " " (butlast tokens))) 2)

      (#{"cubic" "cubed"} (last lower-tokens))
      (unit-map (parse-unit-phrase (str/join " " (butlast tokens))) 3)

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
  "Split tokens on *, /, +, or - that appear between quantity groups.
   A token is a quantity operator when it is followed by something that
   starts a number.  Returns {:segments [[tok ...] ...] :ops [:* :/ :+ or :- ...]}
   or nil when no quantity-level operators are found."
  [tokens]
  (loop [i 0, current [], segments [], ops []]
    (cond
      (>= i (count tokens))
      (when (seq ops)
        {:segments (conj segments current) :ops ops})

      (and (#{"*" "/" "+" "-"} (nth tokens i))
           (seq current)
           (< (inc i) (count tokens))
           (some? (parse-number-at tokens (inc i))))
      (recur (inc i) [] (conj segments current)
             (conj ops (case (nth tokens i)
                         "*" :*
                         "/" :/
                         "+" :+
                         "-" :-)))

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
                       ;; For +/- all operands must have units — otherwise
                       ;; it's plain scalar math (e.g. "3 + 2 hours").
                       ;; For * the first operand must have a unit, unless
                       ;; there's also a / (e.g. "4.5 / 77 days/meter").
                       (if (every? #{:+ :-} ops)
                         (every? #(not= {} (:unit %)) parsed)
                         (or (not= {} (:unit (first parsed)))
                             (some #{:/} ops))))
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
    ;; how many inches [are] in 3 feet
    (re-matches #"(?i)^how many .+(?:\s+are)? in .+$" s)
    (let [[_ to quantity] (re-matches #"(?i)^how many (.+?)(?:\s+are)? in (.+)$" s)]
      [quantity to])

    ;; how many yards is/are 12 feet
    (re-matches #"(?i)^how many .+ (?:is|are) .+$" s)
    (let [[_ to quantity] (re-matches #"(?i)^how many (.+) (?:is|are) (.+)$" s)]
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

(defn split-display-parts
  "Extract {:from ... :target ...} from the input, where :from is the
   quantity side and :target is the unit-only side. Used for display formatting."
  [input]
  (or
   ;; how many X [are] in Y → from=Y, target=X
   (when-let [[_ to from] (re-matches #"(?i)^how many (.+?)(?:\s+are)? in (.+)$" input)]
     {:from (str/trim from) :target (str/trim to)})
   ;; how many X is/are Y → from=Y, target=X
   (when-let [[_ to from] (re-matches #"(?i)^how many (.+) (?:is|are) (.+)$" input)]
     {:from (str/trim from) :target (str/trim to)})
   ;; Y is how many X → from=Y, target=X
   (when-let [[_ from to] (re-matches #"(?i)^(.+) is how many (.+)$" input)]
     {:from (str/trim from) :target (str/trim to)})
   ;; Generic "X in/to Y" (with optional leading "how much is"/"what is"/"convert")
   (when-let [[_ lhs _ rhs] (re-matches #"(?i)^(?:(?:how much is|what is|convert)\s+)?(.+?)\s+(in|to)\s+(.+?)$" input)]
     {:from (str/trim lhs) :target (str/trim rhs)})))

(defn- parse-percentage-number
  "Parse a number from the start of a string, returning [value remaining-str] or nil."
  [s]
  (let [s (str/trim s)
        tokens (->> (str/split s #"\s+") (remove str/blank?) vec)]
    (when-let [[value j] (parse-number-at tokens 0)]
      [value (str/trim (str/join " " (subvec tokens j)))])))

(defn parse-percentage
  "Try to parse a percentage expression. Returns a request map or nil.
   Supports:
     'X is what percent of Y'  → {:op :percentage :type :what-percent :value X :total Y}
     'what percent of Y is X'  → same
     'what percentage of Y is X' → same
     'X percent of Y'          → {:op :percentage :type :percent-of :percent X :value Y}
     'X% of Y'                 → same (% normalized to 'percent' by clean-phrase)"
  [s]
  (let [lower (str/lower-case s)]
    (or
     ;; "X is what percent of Y" / "X is what percentage of Y"
     (when-let [[_ x-str y-str] (re-matches #"(?i)^(.+?)\s+is\s+what\s+percent(?:age)?\s+of\s+(.+)$" s)]
       (when-let [[x _] (parse-percentage-number x-str)]
         (when-let [[y _] (parse-percentage-number y-str)]
           {:op :percentage :type :what-percent :value x :total y})))

     ;; "what percent of Y is X" / "what percentage of Y is X"
     (when-let [[_ y-str x-str] (re-matches #"(?i)^what\s+percent(?:age)?\s+of\s+(.+?)\s+is\s+(.+)$" s)]
       (when-let [[x _] (parse-percentage-number x-str)]
         (when-let [[y _] (parse-percentage-number y-str)]
           {:op :percentage :type :what-percent :value x :total y})))

     ;; "what is X percent of Y"
     (when-let [[_ x-str y-str] (re-matches #"(?i)^what\s+is\s+(.+?)\s+percent\s+of\s+(.+)$" s)]
       (when-let [[x _] (parse-percentage-number x-str)]
         (when-let [[y _] (parse-percentage-number y-str)]
           {:op :percentage :type :percent-of :percent x :value y})))

     ;; "X percent of Y"
     (when-let [[_ x-str y-str] (re-matches #"(?i)^(.+?)\s+percent\s+of\s+(.+)$" s)]
       (when-let [[x _] (parse-percentage-number x-str)]
         (when-let [[y _] (parse-percentage-number y-str)]
           {:op :percentage :type :percent-of :percent x :value y}))))))

(def ^:private ordinal-to-int
  {"2nd" 2 "3rd" 3 "4th" 4 "5th" 5 "6th" 6 "7th" 7 "8th" 8 "9th" 9 "10th" 10
   "second" 2 "third" 3 "fourth" 4 "fifth" 5 "sixth" 6 "seventh" 7 "eighth" 8 "ninth" 9 "tenth" 10})

(defn parse-root
  "Try to parse a root expression. Returns a request map or nil.
   Supports:
     'square root of 144'          → {:op :root :degree 2 :value 144}
     'cube root of 27'             → {:op :root :degree 3 :value 27}
     'sqrt 144' / 'sqrt of 144'    → {:op :root :degree 2 :value 144}
     'cbrt 27' / 'cbrt of 27'      → {:op :root :degree 3 :value 27}
     '4th root of 16'              → {:op :root :degree 4 :value 16}
     'what is the square root of 25' → {:op :root :degree 2 :value 25}
     'fifth root of 32'            → {:op :root :degree 5 :value 32}"
  [s]
  (let [lower (str/lower-case s)]
    (or
     ;; "what is the Nth root of X" / "what is the square root of X"
     (when-let [[_ deg-str val-str] (re-matches #"(?i)^(?:what\s+is\s+(?:the\s+)?)(.+?)\s+root\s+of\s+(.+)$" s)]
       (let [deg-lower (str/lower-case (str/trim deg-str))]
         (when-let [degree (case deg-lower
                             "square" 2
                             "sq" 2
                             "cube" 3
                             (or (get ordinal-to-int deg-lower)
                                 (parse-number-token deg-lower)))]
           (when-let [[value _] (parse-percentage-number val-str)]
             {:op :root :degree (long degree) :value value}))))

     ;; "square root of X"
     (when-let [[_ val-str] (re-matches #"(?i)^square\s+root\s+of\s+(.+)$" s)]
       (when-let [[value _] (parse-percentage-number val-str)]
         {:op :root :degree 2 :value value}))

     ;; "cube root of X"
     (when-let [[_ val-str] (re-matches #"(?i)^cube\s+root\s+of\s+(.+)$" s)]
       (when-let [[value _] (parse-percentage-number val-str)]
         {:op :root :degree 3 :value value}))

     ;; "Nth root of X" (ordinal or numeric)
     (when-let [[_ deg-str val-str] (re-matches #"(?i)^(\S+)\s+root\s+of\s+(.+)$" s)]
       (let [deg-lower (str/lower-case deg-str)]
         (when-let [degree (or (get ordinal-to-int deg-lower)
                               (parse-number-token deg-lower))]
           (when-let [[value _] (parse-percentage-number val-str)]
             {:op :root :degree (long degree) :value value}))))

     ;; "sqrt X" / "sqrt of X"
     (when-let [[_ val-str] (re-matches #"(?i)^sqrt\s+(?:of\s+)?(.+)$" s)]
       (when-let [[value _] (parse-percentage-number val-str)]
         {:op :root :degree 2 :value value}))

     ;; "cbrt X" / "cbrt of X"
     (when-let [[_ val-str] (re-matches #"(?i)^cbrt\s+(?:of\s+)?(.+)$" s)]
       (when-let [[value _] (parse-percentage-number val-str)]
         {:op :root :degree 3 :value value})))))

(defn parse-modulo
  "Try to parse a modulo expression. Returns a request map or nil.
   Supports:
     'X mod Y'     → {:op :modulo :dividend X :divisor Y}
     'X modulo Y'  → same
     'what is X mod Y' → same"
  [s]
  (or
   (when-let [[_ x-str y-str] (re-matches #"(?i)^(?:what\s+is\s+)?(.+?)\s+mod(?:ulo)?\s+(.+)$" s)]
     (when-let [[x _] (parse-percentage-number x-str)]
       (when-let [[y _] (parse-percentage-number y-str)]
         {:op :modulo :dividend x :divisor y})))))

(defn parse-request [phrase]
  (let [original phrase]
    (try
      (let [cleaned (evaluate-math-exprs (clean-phrase phrase))
            [without-format format] (extract-format cleaned)
            [without-approx approx?] (extract-approx without-format)
            pct (parse-percentage without-approx)
            root (when-not pct (parse-root without-approx))
            modulo (when-not (or pct root) (parse-modulo without-approx))]
        (if pct
          (cond-> pct
            format (assoc :format format))
        (if root
          (cond-> root
            format (assoc :format format))
        (if modulo
          (cond-> modulo
            format (assoc :format format))
          (let [pieces (split-request without-approx)]
        (if-not pieces
          ;; No "in"/"to" target — try as a standalone quantity expression
          (let [qty (try (parse-quantity without-approx)
                         #?(:clj (catch Exception _ nil)
                            :cljs (catch :default _ nil)))]
            (if (and qty (or (:qty-expr qty)
                             (and (map? qty) (:unit qty) (not= {} (:unit qty)))))
              (cond-> {:op :convert :quantity qty :to :auto}
                approx? (assoc :approx? true)
                format (assoc :format format))
              {:error :unparseable
               :phrase original}))
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
                request)))))))))
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
