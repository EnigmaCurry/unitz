(ns unitz.core
  #?(:clj (:import [java.math BigDecimal MathContext RoundingMode]))
  (:require [unitz.parser :as parser]))

;; ============================================================================
;; Legacy ratio-based unit system (JVM/Babashka only)
;; ============================================================================

#?(:clj
   (def units
     {:m {:kind :length :dimension {:length 1} :factor 1}
      :ft {:kind :length :dimension {:length 1} :factor 381/1250}
      :yd {:kind :length :dimension {:length 1} :factor 1143/1250}
      :mile {:kind :length :dimension {:length 1} :factor 201168/125}
      :m2 {:kind :area :dimension {:length 2} :factor 1}
      :ft2 {:kind :area :dimension {:length 2} :factor (* 381/1250 381/1250)}
      :acre {:kind :area :dimension {:length 2} :factor 40468564224/10000000}
      :m3 {:kind :volume :dimension {:length 3} :factor 1}
      :liter {:kind :volume :dimension {:length 3} :factor 1/1000}
      :L {:kind :volume :dimension {:length 3} :factor 1/1000}
      :ml {:kind :volume :dimension {:length 3} :factor 1/1000000}
      :gal {:kind :volume :dimension {:length 3} :factor 473176473/125000000000}
      :kg {:kind :mass :dimension {:mass 1} :factor 1}
      :g {:kind :mass :dimension {:mass 1} :factor 1/1000}
      :lb {:kind :mass :dimension {:mass 1} :factor 45359237/100000000}
      :oz {:kind :mass :dimension {:mass 1} :factor 45359237/1600000000}
      :s {:kind :time :dimension {:time 1} :factor 1}
      :min {:kind :time :dimension {:time 1} :factor 60}
      :hour {:kind :time :dimension {:time 1} :factor 3600}
      :day {:kind :time :dimension {:time 1} :factor 86400}
      :week {:kind :time :dimension {:time 1} :factor 604800}
      :year {:kind :time :dimension {:time 1} :factor 31557600}
      :century {:kind :time :dimension {:time 1} :factor 3155760000}
      :millennium {:kind :time :dimension {:time 1} :factor 31557600000}
      :mps {:kind :speed :dimension {:length 1 :time -1} :factor 1}
      :kph {:kind :speed :dimension {:length 1 :time -1} :factor 5/18}
      :fps {:kind :speed :dimension {:length 1 :time -1} :factor 381/1250}
      :mph {:kind :speed :dimension {:length 1 :time -1} :factor (/ 201168/125 3600)}
      :mps2 {:kind :acceleration :dimension {:length 1 :time -2} :factor 1}
      :N {:kind :force :dimension {:mass 1 :length 1 :time -2} :factor 1}
      :J {:kind :energy :dimension {:mass 1 :length 2 :time -2} :factor 1}
      :W {:kind :power :dimension {:mass 1 :length 2 :time -3} :factor 1}
      :A {:kind :electric-current :dimension {:current 1} :factor 1}
      :K {:kind :temperature :dimension {:temperature 1} :factor 1}
      :mol {:kind :amount :dimension {:amount 1} :factor 1}
      :cd {:kind :luminous-intensity :dimension {:luminous 1} :factor 1}}))

#?(:clj
   (defn unit
     "Look up unit metadata for unit keyword `u`."
     [u]
     (or (get units u)
         (throw (ex-info "Unknown unit" {:unit u})))))

(defn clean-exponents
  "Remove zero-valued exponents from dimensional exponent map `m`."
  [m]
  (->> m
       (remove (fn [[_ exponent]] (zero? exponent)))
       (into {})))

(defn merge-exponents
  "Merge one or more dimensional exponent maps by adding matching exponents."
  [& maps]
  (clean-exponents
   (apply merge-with + maps)))

#?(:clj
   (defn invert-unit
     "Invert a unit metadata map."
     [u]
     {:dimension (update-vals (:dimension u) -)
      :factor (/ 1 (:factor u))}))

#?(:clj
   (defn multiply-units
     "Multiply one or more unit metadata maps."
     [& us]
     {:dimension (apply merge-exponents (map :dimension us))
      :factor (apply * (map :factor us))}))

#?(:clj
   (defn divide-units
     "Divide unit metadata map `a` by unit metadata map `b`."
     [a b]
     (multiply-units a (invert-unit b))))

#?(:clj
   (defn compatible-resolved-units?
     "Return true if two resolved unit metadata maps have the same dimension."
     [from to]
     (= (:dimension from) (:dimension to))))

#?(:clj
   (defn convert-resolved-units
     "Convert numeric `value` between two resolved unit metadata maps."
     [value from to]
     (when-not (compatible-resolved-units? from to)
       (throw (ex-info "Incompatible units"
                       {:from-dimension (:dimension from)
                        :to-dimension (:dimension to)})))
     (/ (* value (:factor from))
        (:factor to))))

#?(:clj
   (defn unit-expr
     "Resolve a unit expression into a unit metadata map."
     [expr]
     (cond
       (keyword? expr)
       (unit expr)

       (vector? expr)
       (let [[op a b] expr]
         (case op
           :* (multiply-units (unit-expr a) (unit-expr b))
           :/ (divide-units (unit-expr a) (unit-expr b))
           (throw (ex-info "Unknown unit expression operator"
                           {:operator op :expr expr}))))

       :else
       (throw (ex-info "Invalid unit expression" {:expr expr})))))

#?(:clj
   (defn convert
     "Convert numeric `value` from unit expression `from` to unit expression `to`."
     [value from to]
     (convert-resolved-units value (unit-expr from) (unit-expr to))))

;; ============================================================================
;; BigDecimal-based unit system (cross-platform request pipeline)
;; ============================================================================

#?(:clj (def ^:private math-context MathContext/DECIMAL128))

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
  #?(:clj
     (cond
       (or (instance? BigDecimal a)
           (instance? BigDecimal b))
       (.divide (bigdec a) (bigdec b) math-context)

       :else
       (/ a b))
     :cljs
     (/ a b)))

#?(:clj (def ^:private output-scale 14))

(defn normalize-number [x]
  #?(:clj
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
       x)
     :cljs
     (if (js/Number.isInteger x)
       (int x)
       ;; Round to 12 significant digits to avoid floating point noise
       (js/parseFloat (.toPrecision (js/Number x) 12)))))

(defn- ->bigdec
  "Convert a number to BigDecimal on JVM (using valueOf for exact representation).
   Identity on CLJS."
  [x]
  #?(:clj
     (if (integer? x)
       (BigDecimal/valueOf (long x))
       (BigDecimal/valueOf (double x)))
     :cljs x))

(def unit-defs
  ;; :scale means "multiply by this to get SI/base dimensions".
  ;; Scales stored as plain numbers, converted to BigDecimal on JVM at load time.
  {:m    {:dim {:length 1} :scale (->bigdec 1)}
   :km   {:dim {:length 1} :scale (->bigdec 1000)}
   :cm   {:dim {:length 1} :scale (->bigdec 0.01)}
   :ft   {:dim {:length 1} :scale (->bigdec 0.3048)}
   :yd   {:dim {:length 1} :scale (->bigdec 0.9144)}
   :in   {:dim {:length 1} :scale (->bigdec 0.0254)}
   :mi   {:dim {:length 1} :scale (->bigdec 1609.344)}

   :kg   {:dim {:mass 1} :scale (->bigdec 1)}
   :g    {:dim {:mass 1} :scale (->bigdec 0.001)}
   :lb   {:dim {:mass 1} :scale (->bigdec 0.45359237)}
   :oz   {:dim {:mass 1} :scale (->bigdec 0.028349523125)}

   :s    {:dim {:time 1} :scale (->bigdec 1)}
   :min  {:dim {:time 1} :scale (->bigdec 60)}
   :hr   {:dim {:time 1} :scale (->bigdec 3600)}
   :day  {:dim {:time 1} :scale (->bigdec 86400)}
   :week {:dim {:time 1} :scale (->bigdec 604800)}
   :yr   {:dim {:time 1} :scale (->bigdec 31557600)}
   :century {:dim {:time 1} :scale (->bigdec 3155760000)}
   :millennium {:dim {:time 1} :scale (->bigdec 31557600000)}

   ;; Volume as length^3, using cubic meters as base.
   :l    {:dim {:length 3} :scale (->bigdec 0.001)}
   :ml   {:dim {:length 3} :scale (->bigdec 0.000001)}

   ;; US liquid gallon.
   :gal  {:dim {:length 3} :scale (->bigdec 0.003785411784)}
   :cup  {:dim {:length 3} :scale (->bigdec 0.0002365882365)}
   :tbsp {:dim {:length 3} :scale (->bigdec 0.00001478676478125)}
   :tsp  {:dim {:length 3} :scale (->bigdec 0.00000492892159375)}

   :acre {:dim {:length 2} :scale (->bigdec 4046.8564224)}

   ;; Data. Base is byte.
   :bit  {:dim {:data 1} :scale (->bigdec 0.125)}

   ;; Bytes (decimal / SI)
   :B    {:dim {:data 1} :scale (->bigdec 1)}
   :KB   {:dim {:data 1} :scale (->bigdec 1000)}
   :MB   {:dim {:data 1} :scale (->bigdec 1000000)}
   :GB   {:dim {:data 1} :scale (->bigdec 1000000000)}
   :TB   {:dim {:data 1} :scale (->bigdec 1000000000000)}
   :PB   {:dim {:data 1} :scale (->bigdec 1000000000000000)}
   :EB   {:dim {:data 1} :scale (->bigdec 1000000000000000000)}

   ;; Bytes (binary / IEC)
   :KiB  {:dim {:data 1} :scale (->bigdec 1024)}
   :MiB  {:dim {:data 1} :scale (->bigdec 1048576)}
   :GiB  {:dim {:data 1} :scale (->bigdec 1073741824)}
   :TiB  {:dim {:data 1} :scale (->bigdec 1099511627776)}
   :PiB  {:dim {:data 1} :scale (->bigdec 1125899906842624)}
   :EiB  {:dim {:data 1} :scale (->bigdec 1152921504606846976)}

   ;; Bits (decimal)
   :Kb   {:dim {:data 1} :scale (->bigdec 125)}
   :Mb   {:dim {:data 1} :scale (->bigdec 125000)}
   :Gb   {:dim {:data 1} :scale (->bigdec 125000000)}
   :Tb   {:dim {:data 1} :scale (->bigdec 125000000000)}
   :Pb   {:dim {:data 1} :scale (->bigdec 125000000000000)}
   :Eb   {:dim {:data 1} :scale (->bigdec 125000000000000000)}

   ;; Derived mechanical units.
   :N    {:dim {:mass 1 :length 1 :time -2} :scale (->bigdec 1)}
   :J    {:dim {:mass 1 :length 2 :time -2} :scale (->bigdec 1)}
   :W    {:dim {:mass 1 :length 2 :time -3} :scale (->bigdec 1)}
   :Pa   {:dim {:mass 1 :length -1 :time -2} :scale (->bigdec 1)}
   :psi  {:dim {:mass 1 :length -1 :time -2} :scale (->bigdec 6894.757293168)}})

(def temperature-units
  #{:degF :degC :K})

(defn pow-dec [x n]
  (cond
    (= n 0)
    (->bigdec 1)

    (pos? n)
    (reduce * (->bigdec 1) (repeat n x))

    :else
    (safe-div (->bigdec 1) (pow-dec x (- n)))))

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
     {:dim {} :scale (->bigdec 1)}
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

(def ^:private temp-offset (->bigdec 273.15))
(def ^:private temp-32 (->bigdec 32))
(def ^:private temp-5 (->bigdec 5))
(def ^:private temp-9 (->bigdec 9))

(defn c->k [c]
  (+ c temp-offset))

(defn k->c [k]
  (- k temp-offset))

(defn f->c [f]
  (* (- f temp-32) (safe-div temp-5 temp-9)))

(defn c->f [c]
  (+ (* c (safe-div temp-9 temp-5)) temp-32))

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
         total 0]
    (if (empty? remaining)
      (normalize-number total)
      (let [converted (convert-one (first remaining) to-unit)]
        (if (error? converted)
          converted
          (recur (rest remaining) (+ total converted)))))))

(defn- coerce-to-decimal [x]
  #?(:clj (bigdec x)
     :cljs x))

(defn- evaluate-qty-expr
  "Evaluate quantity arithmetic: multiply/divide quantities with units,
   then convert the result to `to-unit`."
  [{:keys [terms ops]} to-unit]
  (let [first-spec (unit-spec (:unit (first terms)))
        result (reduce
                (fn [{:keys [value dim]} [op term]]
                  (let [spec     (unit-spec (:unit term))
                        term-val (* (coerce-to-decimal (:value term)) (:scale spec))]
                    (case op
                      :* {:value (* value term-val)
                          :dim   (merge-dims dim (:dim spec))}
                      :/ {:value (safe-div value term-val)
                          :dim   (merge-dims dim (scale-dim (:dim spec) -1))})))
                {:value (* (coerce-to-decimal (:value (first terms))) (:scale first-spec))
                 :dim   (:dim first-spec)}
                (map vector ops (rest terms)))
        to-spec (unit-spec to-unit)]
    (if (= (:dim result) (:dim to-spec))
      (normalize-number (safe-div (:value result) (:scale to-spec)))
      {:error :incompatible-dimensions
       :from  (:dim result)
       :to    (:dim to-spec)})))

(defn convert-request [{:keys [op quantity to] :as request}]
  (cond
    (error? request)
    request

    (not= op :convert)
    {:error :unsupported-operation
     :op op}

    (:qty-expr quantity)
    (evaluate-qty-expr quantity to)

    (vector? quantity)
    (convert-mixed quantity to)

    (map? quantity)
    (convert-one quantity to)

    :else
    {:error :invalid-request
     :request request}))
