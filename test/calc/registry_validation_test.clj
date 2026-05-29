(ns calc.registry-validation-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [calc.core :as u]))

(deftest every-unit-has-name-and-short
  (testing "every unit has :name and :short fields"
    (doseq [[unit metadata] u/unit-defs]
      (is (string? (:name metadata))
          (str unit " is missing :name"))
      (is (string? (:short metadata))
          (str unit " is missing :short")))))

(deftest every-unit-has-aliases
  (testing "every unit has a non-empty :aliases vector"
    (doseq [[unit metadata] u/unit-defs]
      (is (vector? (:aliases metadata))
          (str unit " :aliases should be a vector"))
      (is (seq (:aliases metadata))
          (str unit " has no aliases")))))

(deftest no-duplicate-aliases
  (testing "no alias string maps to more than one unit"
    (let [alias-pairs (for [[unit-key {:keys [aliases]}] u/unit-defs
                            alias aliases]
                        [alias unit-key])
          grouped (group-by first alias-pairs)]
      (doseq [[alias entries] grouped]
        (let [units (map second entries)]
          (is (apply = units)
              (str "alias \"" alias "\" maps to multiple units: "
                   (pr-str (distinct units)))))))))

(deftest no-lowercase-alias-collisions
  (testing "lowercased aliases do not collide across different units (except intentional case-sensitive data units)"
    (let [;; Data units are intentionally case-sensitive (B vs b, KB vs Kb, etc.)
          ;; so we expect lowercase collisions within the data dimension.
          ;; We check that collisions only happen within the same dimension.
          alias-pairs (for [[unit-key {:keys [aliases]}] u/unit-defs
                            alias aliases]
                        [(str/lower-case alias) unit-key])
          grouped (group-by first alias-pairs)]
      (doseq [[lc-alias entries] grouped]
        (let [units (distinct (map second entries))
              dims  (map #(or (:dim (get u/unit-defs %))
                              (when (:temperature (get u/unit-defs %))
                                :temperature))
                         units)]
          ;; If multiple units share a lowercased alias, they should be
          ;; in the same dimension (e.g. KB and Kb are both {:data 1})
          (when (> (count units) 1)
            (is (apply = dims)
                (str "lowercased alias \"" lc-alias "\" maps to units in different dimensions: "
                     (pr-str (zipmap units dims))))))))))

(deftest non-temperature-units-have-positive-scale
  (testing "every non-temperature unit has a positive scale factor"
    (doseq [[unit metadata] u/unit-defs
            :when (not (:temperature metadata))]
      (is (pos? (:scale metadata))
          (str unit " has non-positive :scale: " (:scale metadata))))))

(deftest temperature-units-have-no-dim-or-scale
  (testing "temperature units must not have :dim or :scale"
    (doseq [[unit metadata] u/unit-defs
            :when (:temperature metadata)]
      (is (not (contains? metadata :dim))
          (str unit " is a temperature unit but has :dim"))
      (is (not (contains? metadata :scale))
          (str unit " is a temperature unit but has :scale")))))

(deftest every-dimension-has-a-category
  (testing "every dimension present in unit-defs appears in dim-categories"
    (let [all-dims (into #{}
                         (for [[_ metadata] u/unit-defs
                               :when (:dim metadata)]
                           (:dim metadata)))]
      (doseq [dim all-dims]
        (is (contains? u/dim-categories dim)
            (str "dimension " (pr-str dim) " has no entry in dim-categories"))))))

(deftest auto-scale-ordering-is-monotonic
  (testing "auto-scale candidate lists are ordered by increasing scale"
    (let [as-entries (for [[k v] u/unit-defs
                           :when (and (:auto-scale v) (:dim v))]
                       [k v])
          by-dim (group-by (fn [[_ v]] (:dim v)) as-entries)]
      (doseq [[dim entries] by-dim]
        (let [scales (map (fn [[_ v]] (:scale v))
                          (sort-by (fn [[_ v]] (double (:scale v))) entries))]
          (is (= scales (sort-by double scales))
              (str "auto-scale units for " (pr-str dim)
                   " are not in monotonic order")))))))

(deftest auto-scale-dimensions-have-candidates
  (testing "auto-scale dimensions have at least 1 candidate unit"
    (let [as-entries (for [[k v] u/unit-defs
                           :when (and (:auto-scale v) (:dim v))]
                       [k v])
          by-dim (group-by (fn [[_ v]] (:dim v)) as-entries)]
      (doseq [[dim entries] by-dim]
        (is (>= (count entries) 1)
            (str "auto-scale dimension " (pr-str dim)
                 " has no candidates"))))))

(deftest unit-groups-cover-all-units
  (testing "every unit in unit-defs appears in unit-groups"
    (let [grouped-keys (into #{}
                             (for [{:keys [units]} u/unit-groups
                                   [k _] units]
                               k))
          all-keys (set (keys u/unit-defs))]
      (doseq [k all-keys]
        (is (contains? grouped-keys k)
            (str k " is in unit-defs but missing from unit-groups")))))
  (testing "every unit in unit-groups exists in unit-defs"
    (doseq [{:keys [units]} u/unit-groups
            [k _] units]
      (is (contains? u/unit-defs k)
          (str k " is in unit-groups but missing from unit-defs")))))
