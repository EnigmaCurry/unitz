(ns calc.eval
  (:require [calc.units :as u]
            [clojure.string :as str]))

;; ============================================================================
;; Helpers
;; ============================================================================

(defn format-unit-label
  "Format a unit keyword or exponent map as a short canonical string."
  [unit]
  (if (keyword? unit)
    (get u/unit-short-names unit (name unit))
    (let [pos (into {} (filter (fn [[_ v]] (pos? v))) unit)
          neg (into {} (filter (fn [[_ v]] (neg? v))) unit)]
      (str (str/join "\u00b7" (for [[k v] pos]
                                (let [label (get u/unit-short-names k (name k))]
                                  (if (= v 1) label (str label "^" v)))))
           (when (seq neg)
             (str "/" (str/join "\u00b7" (for [[k v] neg]
                                          (let [label (get u/unit-short-names k (name k))]
                                            (if (= v -1) label (str label "^" (- v))))))))))))

;; ============================================================================
;; Scalar conversion
;; ============================================================================

(defn convert-scalar [value from-unit to-unit]
  (if-not (u/compatible? from-unit to-unit)
    (u/incompatible-error from-unit to-unit)
    (let [{from-scale :scale} (u/unit-spec from-unit)
          {to-scale :scale}   (u/unit-spec to-unit)
          value (u/->bigdec value)]
      (u/normalize-number
       (u/safe-div (* value from-scale) to-scale)))))

;; ============================================================================
;; Temperature conversion (affine transforms)
;; ============================================================================

(def ^:private temp-offset (u/->bigdec 273.15))
(def ^:private temp-32 (u/->bigdec 32))
(def ^:private temp-5 (u/->bigdec 5))
(def ^:private temp-9 (u/->bigdec 9))

(defn c->k [c]
  (+ c temp-offset))

(defn k->c [k]
  (- k temp-offset))

(defn f->c [f]
  (* (- f temp-32) (u/safe-div temp-5 temp-9)))

(defn c->f [c]
  (+ (* c (u/safe-div temp-9 temp-5)) temp-32))

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
  (if-not (and (u/temperature-units from-unit)
               (u/temperature-units to-unit))
    (let [temp-dim {:temperature 1}
          from-dim (if (u/temperature-units from-unit) temp-dim (:dim (u/unit-spec from-unit)))
          to-dim   (if (u/temperature-units to-unit)   temp-dim (:dim (u/unit-spec to-unit)))]
      {:error :incompatible-dimensions :from from-dim :to to-dim})
    (u/normalize-number
     (-> value
         (temperature->kelvin from-unit)
         (kelvin->temperature to-unit)))))

(defn temperature-request? [from-unit to-unit]
  (or (u/temperature-units from-unit)
      (u/temperature-units to-unit)))

;; ============================================================================
;; Composite conversion
;; ============================================================================

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
      (u/normalize-number total)
      (let [converted (convert-one (first remaining) to-unit)]
        (if (error? converted)
          converted
          (recur (rest remaining) (+ total converted)))))))

;; ============================================================================
;; Quantity arithmetic evaluation
;; ============================================================================

(defn- coerce-to-decimal [x]
  #?(:clj (bigdec x)
     :cljs x))

(defn- evaluate-qty-expr
  "Evaluate quantity arithmetic: multiply/divide/add/subtract quantities with units,
   then convert the result to `to-unit`."
  [{:keys [terms ops]} to-unit]
  (let [first-spec (u/unit-spec (:unit (first terms)))
        result (reduce
                (fn [{:keys [value dim]} [op term]]
                  (let [spec     (u/unit-spec (:unit term))
                        term-val (* (coerce-to-decimal (:value term)) (:scale spec))]
                    (case op
                      :* {:value (* value term-val)
                          :dim   (u/merge-dims dim (:dim spec))}
                      :/ {:value (u/safe-div value term-val)
                          :dim   (u/merge-dims dim (u/scale-dim (:dim spec) -1))}
                      :+ (if (= dim (:dim spec))
                           {:value (+ value term-val) :dim dim}
                           (reduced {:error :incompatible-dimensions
                                     :from dim :to (:dim spec)}))
                      :- (if (= dim (:dim spec))
                           {:value (- value term-val) :dim dim}
                           (reduced {:error :incompatible-dimensions
                                     :from dim :to (:dim spec)})))))
                {:value (* (coerce-to-decimal (:value (first terms))) (:scale first-spec))
                 :dim   (:dim first-spec)}
                (map vector ops (rest terms)))]
    (if (:error result)
      result
      (let [to-spec (u/unit-spec to-unit)]
        (if (= (:dim result) (:dim to-spec))
          (u/normalize-number (u/safe-div (:value result) (:scale to-spec)))
          {:error :incompatible-dimensions
           :from  (:dim result)
           :to    (:dim to-spec)})))))

;; ---------------------------------------------------------------------------
;; Auto-scaling: pick the best unit for a given dimension and SI value
;; ---------------------------------------------------------------------------

(defn- pick-best-unit
  "From a sorted candidate list, pick the largest unit where |value/scale| >= 1."
  [candidates abs-val]
  (or (->> candidates
           reverse
           (filter (fn [[_ s]]
                     (>= #?(:clj (double (u/safe-div abs-val s))
                            :cljs (/ abs-val s))
                         1.0)))
           first)
      (first candidates)))

(defn- try-compound-unit
  "Try to decompose `dim` into num-dim / denom-dim where both are in
   auto-scale-units. Returns {:value ... :unit-label \"W/day\"} or nil."
  [dim si-value]
  (let [known-dims (keys u/auto-scale-units)
        candidates
        (for [num-dim   known-dims
              :let [remainder (u/normalize-map
                               (merge-with - dim num-dim))]
              :when (seq remainder)
              :when (every? neg? (vals remainder))
              :let [denom-dim (u/normalize-map
                               (into {} (map (fn [[k v]] [k (- v)]) remainder)))]
              :when (get u/auto-scale-units denom-dim)
              :let [num-cands   (get u/auto-scale-units num-dim)
                    denom-cands (get u/auto-scale-units denom-dim)]
              [denom-key denom-scale] denom-cands
              :let [adjusted (* si-value denom-scale)
                    abs-adj  #?(:clj (.abs (bigdec adjusted)) :cljs (js/Math.abs adjusted))
                    [num-key num-scale] (pick-best-unit num-cands abs-adj)
                    converted (u/normalize-number (u/safe-div adjusted num-scale))
                    abs-conv #?(:clj (double (.abs (bigdec converted)))
                                :cljs (js/Math.abs converted))]]
          {:value     converted
           :abs-conv  abs-conv
           :unit-label (str (get u/unit-short-names num-key (name num-key))
                            "/"
                            (get u/unit-short-names denom-key (name denom-key)))})]
    (when (seq candidates)
      (let [score (fn [{:keys [abs-conv]}]
                    (let [range-penalty
                          (cond
                            (and (>= abs-conv 1.0) (< abs-conv 1000.0)) 0.0
                            (< abs-conv 1.0) (Math/log10 (/ 1.0 abs-conv))
                            :else (Math/log10 (/ abs-conv 999.0)))
                          label-penalty (* 0.001 (count (str abs-conv)))]
                      (+ range-penalty label-penalty)))
            best (apply min-key score candidates)]
        (dissoc best :abs-conv)))))

(defn- auto-select-unit
  "Given a dimension map and a value in SI base units, pick the best unit
   and return {:value converted-value :unit-label \"days\"}."
  [dim si-value]
  (if-let [candidates (get u/auto-scale-units dim)]
    (let [abs-val #?(:clj (.abs (bigdec si-value)) :cljs (js/Math.abs si-value))
          [unit-key scale] (pick-best-unit candidates abs-val)
          converted (u/normalize-number (u/safe-div si-value scale))]
      {:value converted :unit-label (get u/unit-display-names unit-key (name unit-key))})
    (or (try-compound-unit dim si-value)
        {:value (u/normalize-number si-value) :unit-label nil})))

(defn- evaluate-qty-expr-auto
  "Evaluate a quantity expression and auto-select the best output unit."
  [{:keys [terms ops]}]
  (let [first-spec (u/unit-spec (:unit (first terms)))
        result (reduce
                (fn [{:keys [value dim]} [op term]]
                  (let [spec     (u/unit-spec (:unit term))
                        term-val (* (coerce-to-decimal (:value term)) (:scale spec))]
                    (case op
                      :* {:value (* value term-val)
                          :dim   (u/merge-dims dim (:dim spec))}
                      :/ {:value (u/safe-div value term-val)
                          :dim   (u/merge-dims dim (u/scale-dim (:dim spec) -1))}
                      :+ (if (= dim (:dim spec))
                           {:value (+ value term-val) :dim dim}
                           (reduced {:error :incompatible-dimensions
                                     :from dim :to (:dim spec)}))
                      :- (if (= dim (:dim spec))
                           {:value (- value term-val) :dim dim}
                           (reduced {:error :incompatible-dimensions
                                     :from dim :to (:dim spec)})))))
                {:value (* (coerce-to-decimal (:value (first terms))) (:scale first-spec))
                 :dim   (:dim first-spec)}
                (map vector ops (rest terms)))]
    (if (:error result)
      result
      (auto-select-unit (:dim result) (:value result)))))

;; ============================================================================
;; Request dispatch
;; ============================================================================

(defn- wrap-result
  "Wrap an internal result into a uniform envelope.
   Success: {:ok? true :value N} or {:ok? true :value N :unit-label \"days\"}
            or {:ok? true :mixed [{:value N :unit-label \"ft\"} ...]}
   Error:   {:ok? false :error :kind ...}"
  [result]
  (cond
    (error? result)
    (assoc result :ok? false)

    ;; Mixed output result
    (and (map? result) (contains? result :mixed))
    (assoc result :ok? true)

    ;; Auto-scaled result from evaluate-qty-expr-auto / auto-select-unit
    (and (map? result) (contains? result :value))
    (assoc result :ok? true)

    ;; Bare number from convert-one / convert-mixed / evaluate-qty-expr
    :else
    {:ok? true :value result}))

(defn- evaluate-percentage [{:keys [type value total percent] :as _request}]
  (case type
    :what-percent
    (let [result (u/normalize-number
                  (u/safe-div (* (u/->bigdec value) (u/->bigdec 100)) (u/->bigdec total)))]
      {:value result :unit-label "%"})

    :percent-of
    (let [result (u/normalize-number
                  (u/safe-div (* (u/->bigdec percent) (u/->bigdec value)) (u/->bigdec 100)))]
      {:value result})))

(defn- evaluate-root
  "Compute the nth root of a value. Returns exact integer for perfect roots,
   otherwise BigDecimal (JVM) or float (ClojureScript)."
  [{:keys [degree value]}]
  #?(:clj
     (let [n (long degree)
           v (u/->bigdec value)
           neg? (neg? (double v))
           abs-v (if neg? (.negate v) v)
           approx (Math/pow (double abs-v) (/ 1.0 n))
           candidate (Math/round approx)]
       (if (and (pos? candidate)
                (= (reduce *' (repeat n (bigint candidate)))
                   (bigint abs-v)))
         ;; Perfect root
         (let [result (bigint candidate)]
           {:value (u/normalize-number (if (and neg? (odd? n)) (- result) result))})
         ;; Imperfect root — use decimal
         (let [result (Math/pow (double v) (/ 1.0 n))]
           {:value (u/normalize-number (bigdec result))})))
     :cljs
     (let [result (js/Math.pow value (/ 1.0 degree))]
       (if (== result (js/Math.round result))
         {:value (js/Math.round result)}
         {:value result}))))

(defn- evaluate-modulo [{:keys [dividend divisor]}]
  (let [result (u/normalize-number (mod (u/->bigdec dividend) (u/->bigdec divisor)))]
    {:value result}))

(defn- convert-to-mixed-units
  "Convert a single value+unit to a vector of mixed output units.
   E.g., 180 cm → [{:value 5 :unit-label \"ft\"} {:value 10.866... :unit-label \"in\"}]
   The last unit gets the fractional remainder."
  [value from-unit to-units]
  (let [;; First convert to the first target unit to get the total
        first-to (first to-units)]
    (if (temperature-request? from-unit first-to)
      ;; Temperature doesn't support mixed output
      {:error :incompatible-dimensions
       :from (:dim (u/unit-spec from-unit))
       :to "mixed units"}
      (let [;; Check all target units are compatible
            from-dim (:dim (u/unit-spec from-unit))
            _ (doseq [tu to-units]
                (when (not= from-dim (:dim (u/unit-spec tu)))
                  (throw (ex-info "Incompatible mixed target units"
                                  {:error :incompatible-dimensions
                                   :from from-dim
                                   :to (:dim (u/unit-spec tu))}))))
            ;; Convert source to SI base value
            from-spec (u/unit-spec from-unit)
            si-value (* (coerce-to-decimal value) (:scale from-spec))
            ;; Cascade through target units largest-to-smallest
            results (loop [remaining-si si-value
                           units to-units
                           acc []]
                      (if (= 1 (count units))
                        ;; Last unit gets the remainder
                        (let [u (first units)
                              spec (u/unit-spec u)
                              converted (u/normalize-number
                                         (u/safe-div remaining-si (:scale spec)))]
                          (conj acc {:value converted
                                     :unit-label (format-unit-label u)}))
                        (let [u (first units)
                              spec (u/unit-spec u)
                              converted (u/safe-div remaining-si (:scale spec))
                              whole #?(:clj (bigint (long (Math/floor (double converted))))
                                       :cljs (js/Math.floor converted))
                              used (* (coerce-to-decimal whole) (:scale spec))
                              leftover (- remaining-si used)]
                          (recur leftover
                                 (rest units)
                                 (conj acc {:value (u/normalize-number whole)
                                            :unit-label (format-unit-label u)})))))]
        {:mixed results}))))

(defn convert-request [{:keys [op quantity to] :as request}]
  (wrap-result
   (cond
     (error? request)
     request

     (= op :percentage)
     (evaluate-percentage request)

     (= op :root)
     (evaluate-root request)

     (= op :modulo)
     (evaluate-modulo request)

     (not= op :convert)
     {:error :unsupported-operation
      :op op}

     ;; Mixed output target (e.g., "feet and inches")
     (vector? to)
     (cond
       (vector? quantity)
       ;; Mixed input → mixed output: sum inputs first
       (let [first-to (first to)
             summed (convert-mixed quantity first-to)]
         (if (error? summed)
           summed
           (convert-to-mixed-units summed first-to to)))

       (:qty-expr quantity)
       ;; Quantity expression → mixed output
       (let [first-to (first to)
             result (evaluate-qty-expr quantity first-to)]
         (if (error? result)
           result
           (convert-to-mixed-units result first-to to)))

       (map? quantity)
       (convert-to-mixed-units (:value quantity) (:unit quantity) to)

       :else
       {:error :invalid-request :request request})

     (and (:qty-expr quantity) (= to :auto))
     (evaluate-qty-expr-auto quantity)

     (:qty-expr quantity)
     (evaluate-qty-expr quantity to)

     (and (map? quantity) (= to :auto))
     (let [unit-key (:unit quantity)]
       (if (keyword? unit-key)
         {:value (u/normalize-number (:value quantity))
          :unit-label (get u/unit-display-names unit-key (name unit-key))}
         ;; Compound unit (exponent map) — convert value to SI and auto-select
         (let [spec (u/unit-spec unit-key)
               si-value (* (coerce-to-decimal (:value quantity)) (:scale spec))
               auto-result (auto-select-unit (:dim spec) si-value)]
           (if (:unit-label auto-result)
             auto-result
             ;; No auto-scale match — return original value with label from input unit
             {:value (u/normalize-number (:value quantity))
              :unit-label (format-unit-label unit-key)
              :ok? true}))))

     (vector? quantity)
     (convert-mixed quantity to)

     (map? quantity)
     (convert-one quantity to)

     :else
     {:error :invalid-request
      :request request})))
