;; src/unitz/parser.clj
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

   "b" :B
   "byte" :B
   "bytes" :B

   "kb" :KB
   "kilobyte" :KB
   "kilobytes" :KB

   "mb" :MB
   "megabyte" :MB
   "megabytes" :MB

   "mib" :MiB
   "mebibyte" :MiB
   "mebibytes" :MiB

   "gib" :GiB
   "gibibyte" :GiB
   "gibibytes" :GiB

   "megabit" :Mb
   "megabits" :Mb})

(def special-unit-forms
  {"mph" {:mi 1 :hr -1}
   "mbps" {:Mb 1 :s -1}})

(defn clean-phrase [s]
  (-> s
      str/trim
      (str/replace #"[?]" "")
      (str/replace #"," "")
      (str/replace #"~\s*" "~ ")
      ;; 12ft -> 12 ft, 100kg -> 100 kg
      (str/replace #"(?<=\d)(?=[A-Za-z])" " ")
      (str/replace #"\s+" " ")
      str/trim))

(defn parse-integer [s]
  (bigint s))

(defn parse-ratio-token [s]
  (let [[n d] (str/split s #"/")]
    (/ (parse-integer n)
       (parse-integer d))))

(defn parse-decimal-token [s]
  (bigdec s))

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

(defn parse-number-at [tokens i]
  (let [t (some-> (nth tokens i nil) str/lower-case)
        t2 (some-> (nth tokens (inc i) nil) str/lower-case)]
    (cond
      (#{"a" "an"} t)
      [1N (inc i)]

      (= "half" t)
      [1/2 (inc i)]

      (= "quarter" t)
      [1/4 (inc i)]

      (and (some? (parse-number-token t))
           (some? t2)
           (re-matches #"\d+/\d+" t2))
      [(+ (parse-number-token t)
          (parse-number-token t2))
       (+ i 2)]

      (some? (parse-number-token t))
      [(parse-number-token t) (inc i)]

      :else
      nil)))

(defn normalize-unit-token [s]
  (let [k (-> s
              str/trim
              (str/replace #"^[^\w]+|[^\w]+$" "")
              str/lower-case)]
    (or (get unit-aliases k)
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

(defn parse-component-token [token]
  (let [raw token
        lower (str/lower-case raw)]
    (cond
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
                  (Integer/parseInt exp)))

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
      (let [i (.indexOf lower-tokens "per")
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

(defn parse-quantity [s]
  (let [tokens (->> (str/split (str/trim s) #"\s+")
                    (remove str/blank?)
                    vec)]
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
                              :unit (parse-unit-phrase unit-str)})))))))

(defn extract-format [s]
  (cond
    (re-find #"(?i)\s+rounded to \d+ decimals?$" s)
    (let [[_ n] (re-find #"(?i)\s+rounded to (\d+) decimals?$" s)]
      [(str/replace s #"(?i)\s+rounded to \d+ decimals?$" "")
       {:round (Long/parseLong n)}])

    (re-find #"(?i)\s+with \d+ sig figs$" s)
    (let [[_ n] (re-find #"(?i)\s+with (\d+) sig figs$" s)]
      [(str/replace s #"(?i)\s+with \d+ sig figs$" "")
       {:sig-figs (Long/parseLong n)}])

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

(defn unknown-unit-error [phrase ex]
  (let [data (ex-data ex)]
    (if (= :unknown-unit (:error data))
      {:error :unknown-unit
       :unit (:unit data)
       :phrase phrase}
      (throw ex))))


(defn parse-request [phrase]
  (let [original phrase]
    (try
      (let [cleaned (clean-phrase phrase)
            [without-format format] (extract-format cleaned)
            [without-approx approx?] (extract-approx without-format)
            pieces (split-request without-approx)]
        (if-not pieces
          {:error :unparseable
           :phrase original}
          (let [[quantity-str to-str] pieces
                request (cond-> {:op :convert
                                 :quantity (parse-quantity quantity-str)
                                 :to (parse-unit-phrase to-str)}
                          approx? (assoc :approx? true)
                          format (assoc :format format))]
            request)))
      (catch clojure.lang.ExceptionInfo ex
        (or (unknown-unit-error original ex)
            {:error :unparseable
             :phrase original}))
      (catch Exception _
        {:error :unparseable
         :phrase original}))))
