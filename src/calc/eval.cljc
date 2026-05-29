(ns calc.eval
  (:require [calc.units :as u]))

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
  "Evaluate quantity arithmetic: multiply/divide quantities with units,
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
                          :dim   (u/merge-dims dim (u/scale-dim (:dim spec) -1))})))
                {:value (* (coerce-to-decimal (:value (first terms))) (:scale first-spec))
                 :dim   (:dim first-spec)}
                (map vector ops (rest terms)))
        to-spec (u/unit-spec to-unit)]
    (if (= (:dim result) (:dim to-spec))
      (u/normalize-number (u/safe-div (:value result) (:scale to-spec)))
      {:error :incompatible-dimensions
       :from  (:dim result)
       :to    (:dim to-spec)})))

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
                          :dim   (u/merge-dims dim (u/scale-dim (:dim spec) -1))})))
                {:value (* (coerce-to-decimal (:value (first terms))) (:scale first-spec))
                 :dim   (:dim first-spec)}
                (map vector ops (rest terms)))]
    (auto-select-unit (:dim result) (:value result))))

;; ============================================================================
;; Request dispatch
;; ============================================================================

(defn- wrap-result
  "Wrap an internal result into a uniform envelope.
   Success: {:ok? true :value N} or {:ok? true :value N :unit-label \"days\"}
   Error:   {:ok? false :error :kind ...}"
  [result]
  (cond
    (error? result)
    (assoc result :ok? false)

    ;; Auto-scaled result from evaluate-qty-expr-auto / auto-select-unit
    (and (map? result) (contains? result :value))
    (assoc result :ok? true)

    ;; Bare number from convert-one / convert-mixed / evaluate-qty-expr
    :else
    {:ok? true :value result}))

(defn convert-request [{:keys [op quantity to] :as request}]
  (wrap-result
   (cond
     (error? request)
     request

     (not= op :convert)
     {:error :unsupported-operation
      :op op}

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
               si-value (* (coerce-to-decimal (:value quantity)) (:scale spec))]
           (auto-select-unit (:dim spec) si-value))))

     (vector? quantity)
     (convert-mixed quantity to)

     (map? quantity)
     (convert-one quantity to)

     :else
     {:error :invalid-request
      :request request})))
