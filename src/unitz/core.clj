(ns unitz.core
  (:import [java.math BigDecimal MathContext])
  (:import [java.math BigDecimal MathContext RoundingMode])
  (:require [unitz.parser :as parser]))

(def units
  {;; -------------------------
   ;; Length
   ;; -------------------------

   :m {:kind      :length
       :dimension {:length 1}
       :factor    1}

   :ft {:kind      :length
        :dimension {:length 1}
        :factor    381/1250}

   :yd {:kind      :length
        :dimension {:length 1}
        :factor    1143/1250}

   :mile {:kind      :length
          :dimension {:length 1}
          :factor    201168/125}

   ;; -------------------------
   ;; Area
   ;; -------------------------
   ;;
   ;; Area is not a base dimension.
   ;; It is length^2.

   :m2 {:kind      :area
        :dimension {:length 2}
        :factor    1}

   :ft2 {:kind      :area
         :dimension {:length 2}
         :factor    (* 381/1250 381/1250)}

   :acre {:kind      :area
          :dimension {:length 2}
          :factor    40468564224/10000000}

   ;; -------------------------
   ;; Volume
   ;; -------------------------
   ;;
   ;; Volume is length^3.

   :m3 {:kind      :volume
        :dimension {:length 3}
        :factor    1}

   :liter {:kind      :volume
           :dimension {:length 3}
           :factor    1/1000}

   :L {:kind      :volume
       :dimension {:length 3}
       :factor    1/1000}

   :ml {:kind      :volume
        :dimension {:length 3}
        :factor    1/1000000}

   :gal {:kind      :volume
         :dimension {:length 3}
         :factor    473176473/125000000000}

   ;; -------------------------
   ;; Mass
   ;; -------------------------

   :kg {:kind      :mass
        :dimension {:mass 1}
        :factor    1}

   :g {:kind      :mass
       :dimension {:mass 1}
       :factor    1/1000}

   :lb {:kind      :mass
        :dimension {:mass 1}
        :factor    45359237/100000000}

   :oz {:kind      :mass
        :dimension {:mass 1}
        :factor    45359237/1600000000}

   ;; -------------------------
   ;; Time
   ;; -------------------------

   :s {:kind      :time
       :dimension {:time 1}
       :factor    1}

   :min {:kind      :time
         :dimension {:time 1}
         :factor    60}

   :hour {:kind      :time
          :dimension {:time 1}
          :factor    3600}

   :day {:kind      :time
         :dimension {:time 1}
         :factor    86400}

   :week {:kind      :time
          :dimension {:time 1}
          :factor    604800}

   :year {:kind      :time
          :dimension {:time 1}
          :factor    31557600}

   :century {:kind      :time
             :dimension {:time 1}
             :factor    3155760000}

   :millennium {:kind      :time
                :dimension {:time 1}
                :factor    31557600000}

   ;; -------------------------
   ;; Speed
   ;; -------------------------
   ;;
   ;; Speed is length / time.

   :mps {:kind      :speed
         :dimension {:length 1
                     :time   -1}
         :factor    1}

   :kph {:kind      :speed
         :dimension {:length 1
                     :time   -1}
         :factor    5/18}

   :fps {:kind      :speed
         :dimension {:length 1
                     :time   -1}
         :factor    381/1250}

   :mph {:kind      :speed
         :dimension {:length 1
                     :time   -1}
         :factor    (/ 201168/125 3600)}

   ;; -------------------------
   ;; Acceleration
   ;; -------------------------
   ;;
   ;; Acceleration is length / time^2.

   :mps2 {:kind      :acceleration
          :dimension {:length 1
                      :time   -2}
          :factor    1}

   ;; -------------------------
   ;; Force
   ;; -------------------------
   ;;
   ;; Newton = kg*m/s^2

   :N {:kind      :force
       :dimension {:mass   1
                   :length 1
                   :time   -2}
       :factor    1}

   ;; -------------------------
   ;; Energy
   ;; -------------------------
   ;;
   ;; Joule = kg*m^2/s^2

   :J {:kind      :energy
       :dimension {:mass   1
                   :length 2
                   :time   -2}
       :factor    1}

   ;; -------------------------
   ;; Power
   ;; -------------------------
   ;;
   ;; Watt = kg*m^2/s^3

   :W {:kind      :power
       :dimension {:mass   1
                   :length 2
                   :time   -3}
       :factor    1}

   ;; -------------------------
   ;; Electric current
   ;; -------------------------

   :A {:kind      :electric-current
       :dimension {:current 1}
       :factor    1}

   ;; -------------------------
   ;; Temperature
   ;; -------------------------
   ;;
   ;; For now, only Kelvin works cleanly with factor-only conversion.
   ;; Fahrenheit and Celsius need offsets, so add them later.

   :K {:kind      :temperature
       :dimension {:temperature 1}
       :factor    1}

   ;; -------------------------
   ;; Amount of substance
   ;; -------------------------

   :mol {:kind      :amount
         :dimension {:amount 1}
         :factor    1}

   ;; -------------------------
   ;; Luminous intensity
   ;; -------------------------

   :cd {:kind      :luminous-intensity
        :dimension {:luminous 1}
        :factor    1}})

(defn unit
  "Look up unit metadata for unit keyword `u`.

  Returns a map containing at least:

  - `:dimension` — a dimensional exponent map, such as `{:length 1}`
  - `:factor` — the scale factor relative to the base unit for that dimension

  Throws an `ex-info` exception if `u` is unknown.

  Example:

      (unit :ft)
      ;; => {:dimension {:length 1}, :factor 381/1250}"
  [u]
  (or (get units u)
      (throw (ex-info "Unknown unit" {:unit u}))))

(defn clean-exponents
  "Remove zero-valued exponents from dimensional exponent map `m`.

  This keeps dimension maps canonical by dropping dimensions that cancel out.

  Examples:

      (clean-exponents {:length 2 :time 0})
      ;; => {:length 2}

      (clean-exponents {:length 1 :time -1})
      ;; => {:length 1, :time -1}"
  [m]
  (->> m
       (remove (fn [[_ exponent]]
                 (zero? exponent)))
       (into {})))

(defn merge-exponents
  "Merge one or more dimensional exponent maps by adding matching exponents.

  Dimensions with a resulting exponent of zero are removed.

  This is useful for combining compound units, such as multiplying or dividing
  dimensions.

  Examples:

      (merge-exponents {:length 1} {:time -1})
      ;; => {:length 1, :time -1}

      (merge-exponents {:length 1} {:length -1})
      ;; => {}"
  [& maps]
  (clean-exponents
   (apply merge-with + maps)))

(defn invert-unit
  "Invert a unit metadata map.

  Example:
      meter    => length
      per-sec  => time^-1"
  [u]
  {:dimension (update-vals (:dimension u) -)
   :factor (/ 1 (:factor u))})

(defn multiply-units
  "Multiply one or more unit metadata maps.

  Dimensions are added.
  Scales are multiplied."
  [& us]
  {:dimension (apply merge-exponents (map :dimension us))
   :factor (apply * (map :factor us))})

(defn divide-units
  "Divide unit metadata map `a` by unit metadata map `b`."
  [a b]
  (multiply-units a (invert-unit b)))

(defn compatible-resolved-units?
  "Return true if two resolved unit metadata maps have the same dimension."
  [from to]
  (= (:dimension from)
     (:dimension to)))

(defn convert-resolved-units
  "Convert numeric `value` between two resolved unit metadata maps.

  This is the lower-level conversion function used by `convert`.
  Callers usually want `convert`, which accepts unit expressions."
  [value from to]
  (when-not (compatible-resolved-units? from to)
    (throw (ex-info "Incompatible units"
                    {:from-dimension (:dimension from)
                     :to-dimension (:dimension to)})))
  (/ (* value (:factor from))
     (:factor to)))

(defn unit-expr
  "Resolve a unit expression into a unit metadata map.

  Supported forms:

      :ft
      [:/ :mile :hour]
      [:* :ft :ft]

  Examples:

      (unit-expr :ft)

      (unit-expr [:/ :mile :hour])
      ;; miles per hour

      (unit-expr [:* :ft :ft])
      ;; square feet"
  [expr]
  (cond
    (keyword? expr)
    (unit expr)

    (vector? expr)
    (let [[op a b] expr]
      (case op
        :* (multiply-units (unit-expr a)
                           (unit-expr b))

        :/ (divide-units (unit-expr a)
                         (unit-expr b))

        (throw (ex-info "Unknown unit expression operator"
                        {:operator op
                         :expr expr}))))

    :else
    (throw (ex-info "Invalid unit expression"
                    {:expr expr}))))

(defn convert
  "Convert numeric `value` from unit expression `from` to unit expression `to`.

  Unit expressions may be simple unit keywords:

      :ft
      :yd
      :hour

  Or compound unit expressions:

      [:/ :mile :hour]
      [:/ :ft :s]
      [:* :ft :ft]

  Examples:

      (convert 12 :ft :yd)
      ;; => 4N

      (convert 60 [:/ :mile :hour] [:/ :ft :s])
      ;; => 88N

      (convert 1 [:* :yd :yd] [:* :ft :ft])
      ;; => 9N"
  [value from to]
  (convert-resolved-units value
                          (unit-expr from)
                          (unit-expr to)))

(def math-context MathContext/DECIMAL128)

(def canonical-units
  {:mile :mi
   :miles :mi
   :foot :ft
   :feet :ft
   :yard :yd
   :yards :yd
   :inch :in
   :inches :in
   :meter :m
   :meters :m
   :hour :hr
   :hours :hr
   :second :s
   :seconds :s
   :year :yr
   :years :yr
   :week :week})

(defn canonical-unit [u]
  (get canonical-units u u))

(defn safe-div [a b]
  (cond
    (or (instance? BigDecimal a)
        (instance? BigDecimal b))
    (.divide (bigdec a) (bigdec b) math-context)

    :else
    (/ a b)))

(def output-scale 14)

(defn normalize-number [x]
  (cond
    (integer? x)
    x

    (ratio? x)
    (if (= 1 (denominator x))
      (bigint (numerator x))
      x)

    (instance? BigDecimal x)
    (let [stripped (.stripTrailingZeros x)]
      (try
        (bigint (.toBigIntegerExact stripped))
        (catch ArithmeticException _
          (.stripTrailingZeros
           (.setScale x output-scale RoundingMode/HALF_UP)))))

    :else
    x))

(def unit-defs
  ;; :scale means "multiply by this to get SI/base dimensions".
  {:m    {:dim {:length 1} :scale 1M}
   :km   {:dim {:length 1} :scale 1000M}
   :cm   {:dim {:length 1} :scale 0.01M}
   :ft   {:dim {:length 1} :scale 0.3048M}
   :yd   {:dim {:length 1} :scale 0.9144M}
   :in   {:dim {:length 1} :scale 0.0254M}
   :mi   {:dim {:length 1} :scale 1609.344M}

   :kg   {:dim {:mass 1} :scale 1M}
   :g    {:dim {:mass 1} :scale 0.001M}
   :lb   {:dim {:mass 1} :scale 0.45359237M}
   :oz   {:dim {:mass 1} :scale 0.028349523125M}

   :s    {:dim {:time 1} :scale 1M}
   :min  {:dim {:time 1} :scale 60M}
   :hr   {:dim {:time 1} :scale 3600M}
   :day  {:dim {:time 1} :scale 86400M}
   :week {:dim {:time 1} :scale 604800M}
   :yr   {:dim {:time 1} :scale 31557600M}
   :century {:dim {:time 1} :scale 3155760000M}
   :millennium {:dim {:time 1} :scale 31557600000M}

   ;; Volume as length^3, using cubic meters as base.
   :l    {:dim {:length 3} :scale 0.001M}
   :ml   {:dim {:length 3} :scale 0.000001M}

   ;; US liquid gallon.
   :gal  {:dim {:length 3} :scale 0.003785411784M}
   :cup  {:dim {:length 3} :scale 0.0002365882365M}
   :tbsp {:dim {:length 3} :scale 0.00001478676478125M}
   :tsp  {:dim {:length 3} :scale 0.00000492892159375M}

   :acre {:dim {:length 2} :scale 4046.8564224M}

   ;; Data. Base is byte.
   :B    {:dim {:data 1} :scale 1M}
   :KB   {:dim {:data 1} :scale 1000M}
   :MB   {:dim {:data 1} :scale 1000000M}
   :MiB  {:dim {:data 1} :scale 1048576M}
   :GiB  {:dim {:data 1} :scale 1073741824M}
   :Mb   {:dim {:data 1} :scale 125000M}

   ;; Derived mechanical units.
   :N    {:dim {:mass 1 :length 1 :time -2} :scale 1M}
   :J    {:dim {:mass 1 :length 2 :time -2} :scale 1M}
   :W    {:dim {:mass 1 :length 2 :time -3} :scale 1M}
   :Pa   {:dim {:mass 1 :length -1 :time -2} :scale 1M}
   :psi  {:dim {:mass 1 :length -1 :time -2} :scale 6894.757293168M}})

(def temperature-units
  #{:degF :degC :K})

(defn pow-dec [x n]
  (cond
    (= n 0)
    1M

    (pos? n)
    (reduce * 1M (repeat n x))

    :else
    (safe-div 1M (pow-dec x (- n)))))

(defn normalize-map [m]
  (->> m
       (remove (fn [[_ v]] (zero? v)))
       (into {})))

(defn merge-dims [& dims]
  (normalize-map
   (apply merge-with + dims)))

(defn scale-dim [dim exponent]
  (normalize-map
   (into {} (map (fn [[k v]] [k (* v exponent)]) dim))))

(defn unit-exponent-map [unit]
  (cond
    (keyword? unit)
    {(canonical-unit unit) 1}

    (map? unit)
    (into {}
          (map (fn [[u exp]]
                 [(canonical-unit u) exp]))
          unit)

    ;; Legacy syntax from existing tests:
    ;; [:/ :mile :hour] => {:mi 1, :hr -1}
    (and (vector? unit)
         (= :/ (first unit))
         (= 3 (count unit)))
    (let [[_ num den] unit]
      (merge-with +
                  (unit-exponent-map num)
                  (into {}
                        (map (fn [[u exp]] [u (- exp)]))
                        (unit-exponent-map den))))

    ;; Optional but useful:
    ;; [:* :kg :m] => {:kg 1, :m 1}
    (and (vector? unit)
         (= :* (first unit)))
    (apply merge-with +
           (map unit-exponent-map (rest unit)))

    :else
    (throw (ex-info "Invalid unit form" {:unit unit}))))

(defn unit-spec [unit]
  (let [unit-map (unit-exponent-map unit)]
    (reduce-kv
     (fn [{:keys [dim scale]} u exponent]
       (let [{unit-dim :dim unit-scale :scale} (get unit-defs u)]
         (when-not unit-dim
           (throw (ex-info "Unknown unit" {:unit u})))
         {:dim   (merge-dims dim (scale-dim unit-dim exponent))
          :scale (* scale (pow-dec unit-scale exponent))}))
     {:dim {} :scale 1M}
     unit-map)))

(defn compatible? [from-unit to-unit]
  (= (:dim (unit-spec from-unit))
     (:dim (unit-spec to-unit))))

(defn incompatible-error [from-unit to-unit]
  {:error :incompatible-dimensions
   :from (:dim (unit-spec from-unit))
   :to   (:dim (unit-spec to-unit))})

(defn convert-scalar [value from-unit to-unit]
  (if-not (compatible? from-unit to-unit)
    (incompatible-error from-unit to-unit)
    (let [{from-scale :scale} (unit-spec from-unit)
          {to-scale :scale}   (unit-spec to-unit)]
      (normalize-number
       (safe-div (* value from-scale) to-scale)))))

(defn c->k [c]
  (+ c 273.15M))

(defn k->c [k]
  (- k 273.15M))

(defn f->c [f]
  (* (- f 32M) (safe-div 5M 9M)))

(defn c->f [c]
  (+ (* c (safe-div 9M 5M)) 32M))

(defn temperature->kelvin [value unit]
  (case unit
    :K value
    :degC (c->k value)
    :degF (c->k (f->c value))))

(defn kelvin->temperature [value unit]
  (case unit
    :K value
    :degC (k->c value)
    :degF (c->f (k->c value))))

(defn convert-temperature [value from-unit to-unit]
  (normalize-number
   (if-not (and (temperature-units from-unit)
                (temperature-units to-unit))
     {:error :incompatible-dimensions
      :from {:temperature 1}
      :to (:dim (unit-spec to-unit))}
     (-> value
         (temperature->kelvin from-unit)
         (kelvin->temperature to-unit)))))

(defn temperature-request? [from-unit to-unit]
  (or (temperature-units from-unit)
      (temperature-units to-unit)))

(defn convert-one [{:keys [value unit]} to-unit]
  (if (temperature-request? unit to-unit)
    (convert-temperature value unit to-unit)
    (convert-scalar value unit to-unit)))

(defn error? [x]
  (and (map? x) (contains? x :error)))

(defn convert-mixed [terms to-unit]
  (loop [remaining terms
         total 0M]
    (if (empty? remaining)
      (normalize-number total)
      (let [converted (convert-one (first remaining) to-unit)]
        (if (error? converted)
          converted
          (recur (rest remaining) (+ total converted)))))))

(defn convert-request [{:keys [op quantity to] :as request}]
  (cond
    (error? request)
    request

    (not= op :convert)
    {:error :unsupported-operation
     :op op}

    (vector? quantity)
    (convert-mixed quantity to)

    (map? quantity)
    (convert-one quantity to)

    :else
    {:error :invalid-request
     :request request}))
