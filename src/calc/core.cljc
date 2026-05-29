(ns calc.core
  #?(:clj (:import [java.math BigDecimal MathContext RoundingMode])))

;; ============================================================================
;; BigDecimal-based unit system (cross-platform request pipeline)
;; ============================================================================

#?(:clj (def ^:private math-context MathContext/DECIMAL128))

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

;; ============================================================================
;; Consolidated unit registry
;; ============================================================================
;;
;; Each entry contains:
;;   :dim        - dimension map (e.g. {:length 1})
;;   :scale      - multiply by this to get SI base units
;;   :name       - human-readable display name (for auto-scale output)
;;   :short      - abbreviated name (for compound unit labels like W/day)
;;   :aliases    - vector of English strings the parser recognizes
;;   :auto-scale - true if this unit participates in auto-unit-selection
;;   :temperature - true for temperature units (affine transforms, no dim/scale)

(def unit-defs
  {;; ---- Length ----
   :m      {:dim {:length 1} :scale (->bigdec 1)
            :name "meters" :short "m" :auto-scale true
            :aliases ["m" "meter" "meters"]}
   :km     {:dim {:length 1} :scale (->bigdec 1000)
            :name "km" :short "km" :auto-scale true
            :aliases ["km" "kilometer" "kilometers"]}
   :cm     {:dim {:length 1} :scale (->bigdec 0.01)
            :name "cm" :short "cm" :auto-scale true
            :aliases ["cm" "centimeter" "centimeters"]}
   :mm     {:dim {:length 1} :scale (->bigdec 0.001)
            :name "mm" :short "mm" :auto-scale true
            :aliases ["mm" "millimeter" "millimeters"]}
   :um     {:dim {:length 1} :scale (->bigdec 0.000001)
            :name "μm" :short "μm"
            :aliases ["um" "μm" "micrometer" "micrometers" "micron" "microns"]}
   :nm     {:dim {:length 1} :scale (->bigdec 0.000000001)
            :name "nm" :short "nm"
            :aliases ["nm" "nanometer" "nanometers"]}
   :ft     {:dim {:length 1} :scale (->bigdec 0.3048)
            :name "feet" :short "ft"
            :aliases ["ft" "foot" "feet"]}
   :yd     {:dim {:length 1} :scale (->bigdec 0.9144)
            :name "yards" :short "yd"
            :aliases ["yd" "yard" "yards"]}
   :in     {:dim {:length 1} :scale (->bigdec 0.0254)
            :name "inches" :short "in"
            :aliases ["in" "inch" "inches"]}
   :mi     {:dim {:length 1} :scale (->bigdec 1609.344)
            :name "miles" :short "mi" :auto-scale true
            :aliases ["mi" "mile" "miles"]}
   :nmi    {:dim {:length 1} :scale (->bigdec 1852)
            :name "nautical miles" :short "nmi"
            :aliases ["nmi" "nautical mile" "nautical miles"]}
   :fathom {:dim {:length 1} :scale (->bigdec 1.8288)
            :name "fathoms" :short "fathom"
            :aliases ["fathom" "fathoms"]}
   :ly     {:dim {:length 1} :scale (->bigdec 9460730472580800)
            :name "light-years" :short "ly"
            :aliases ["ly" "lightyear" "lightyears" "light-year" "light-years"]}
   :au     {:dim {:length 1} :scale (->bigdec 149597870700)
            :name "AU" :short "AU"
            :aliases ["au" "AU" "astronomical-unit" "astronomical-units"]}
   :pc     {:dim {:length 1} :scale (->bigdec 30856775814671900)
            :name "parsecs" :short "pc"
            :aliases ["pc" "parsec" "parsecs"]}

   ;; ---- Mass ----
   :kg     {:dim {:mass 1} :scale (->bigdec 1)
            :name "kg" :short "kg" :auto-scale true
            :aliases ["kg" "kilogram" "kilograms"]}
   :g      {:dim {:mass 1} :scale (->bigdec 0.001)
            :name "grams" :short "g" :auto-scale true
            :aliases ["g" "gram" "grams"]}
   :mg     {:dim {:mass 1} :scale (->bigdec 0.000001)
            :name "mg" :short "mg"
            :aliases ["mg" "milligram" "milligrams"]}
   :ug     {:dim {:mass 1} :scale (->bigdec 0.000000001)
            :name "μg" :short "μg"
            :aliases ["ug" "μg" "mcg" "microgram" "micrograms"]}
   :lb     {:dim {:mass 1} :scale (->bigdec 0.45359237)
            :name "pounds" :short "lb" :auto-scale true
            :aliases ["lb" "lbs" "pound" "pounds"]}
   :oz     {:dim {:mass 1} :scale (->bigdec 0.028349523125)
            :name "ounces" :short "oz"
            :aliases ["oz" "ounce" "ounces"]}
   :tonne  {:dim {:mass 1} :scale (->bigdec 1000)
            :name "tonnes" :short "t"
            :aliases ["tonne" "tonnes" "metric ton" "metric tons"]}
   :ton    {:dim {:mass 1} :scale (->bigdec 907.18474)
            :name "tons" :short "ton"
            :aliases ["ton" "tons" "short ton" "short tons"]}
   :stone  {:dim {:mass 1} :scale (->bigdec 6.35029318)
            :name "stone" :short "st"
            :aliases ["stone" "stones" "st"]}
   :ct     {:dim {:mass 1} :scale (->bigdec 0.0002)
            :name "carats" :short "ct"
            :aliases ["ct" "carat" "carats"]}

   ;; ---- Time ----
   :s      {:dim {:time 1} :scale (->bigdec 1)
            :name "seconds" :short "s" :auto-scale true
            :aliases ["s" "sec" "second" "seconds"]}
   :min    {:dim {:time 1} :scale (->bigdec 60)
            :name "minutes" :short "min" :auto-scale true
            :aliases ["min" "minute" "minutes"]}
   :hr     {:dim {:time 1} :scale (->bigdec 3600)
            :name "hours" :short "hr" :auto-scale true
            :aliases ["h" "hr" "hrs" "hour" "hours"]}
   :day    {:dim {:time 1} :scale (->bigdec 86400)
            :name "days" :short "day" :auto-scale true
            :aliases ["day" "days"]}
   :week   {:dim {:time 1} :scale (->bigdec 604800)
            :name "weeks" :short "wk" :auto-scale true
            :aliases ["week" "weeks" "wk"]}
   :yr     {:dim {:time 1} :scale (->bigdec 31557600)
            :name "years" :short "yr" :auto-scale true
            :aliases ["yr" "year" "years"]}
   :century    {:dim {:time 1} :scale (->bigdec 3155760000)
                :name "centuries" :short "century"
                :aliases ["century" "centuries"]}
   :millennium {:dim {:time 1} :scale (->bigdec 31557600000)
                :name "millennia" :short "millennium"
                :aliases ["millennium" "millennia" "millenium" "millenia"
                          "milennium" "milennia"]}

   ;; ---- Volume (length^3) ----
   :l      {:dim {:length 3} :scale (->bigdec 0.001)
            :name "liters" :short "L"
            :aliases ["l" "liter" "liters" "litre" "litres"]}
   :ml     {:dim {:length 3} :scale (->bigdec 0.000001)
            :name "ml" :short "ml"
            :aliases ["ml" "milliliter" "milliliters" "millilitre" "millilitres"]}
   :cc     {:dim {:length 3} :scale (->bigdec 0.000001)
            :name "cc" :short "cc"
            :aliases ["cc" "cubic centimeter" "cubic centimeters"
                      "cubic centimetre" "cubic centimetres"]}
   :gal    {:dim {:length 3} :scale (->bigdec 0.003785411784)
            :name "gallons" :short "gal"
            :aliases ["gal" "gallon" "gallons"]}
   :floz   {:dim {:length 3} :scale (->bigdec 0.0000295735295625)
            :name "fl oz" :short "floz"
            :aliases ["fl oz" "floz" "fluid ounce" "fluid ounces"]}
   :cup    {:dim {:length 3} :scale (->bigdec 0.0002365882365)
            :name "cups" :short "cup"
            :aliases ["cup" "cups"]}
   :pt     {:dim {:length 3} :scale (->bigdec 0.000473176473)
            :name "pints" :short "pt"
            :aliases ["pt" "pint" "pints"]}
   :qt     {:dim {:length 3} :scale (->bigdec 0.000946352946)
            :name "quarts" :short "qt"
            :aliases ["qt" "quart" "quarts"]}
   :tbsp   {:dim {:length 3} :scale (->bigdec 0.00001478676478125)
            :name "tablespoons" :short "tbsp"
            :aliases ["tbsp" "tablespoon" "tablespoons"]}
   :tsp    {:dim {:length 3} :scale (->bigdec 0.00000492892159375)
            :name "teaspoons" :short "tsp"
            :aliases ["tsp" "teaspoon" "teaspoons"]}

   ;; ---- Area (length^2) ----
   :acre   {:dim {:length 2} :scale (->bigdec 4046.8564224)
            :name "acres" :short "acre"
            :aliases ["acre" "acres"]}
   :ha     {:dim {:length 2} :scale (->bigdec 10000)
            :name "hectares" :short "ha"
            :aliases ["ha" "hectare" "hectares"]}

   ;; ---- Data ----
   :bit    {:dim {:data 1} :scale (->bigdec 0.125)
            :name "bits" :short "bit"
            :aliases ["bit" "bits"]}
   :B      {:dim {:data 1} :scale (->bigdec 1)
            :name "bytes" :short "B" :auto-scale true
            :aliases ["B" "byte" "bytes"]}
   :KB     {:dim {:data 1} :scale (->bigdec 1000)
            :name "KB" :short "KB" :auto-scale true
            :aliases ["KB" "kb" "kilobyte" "kilobytes"]}
   :MB     {:dim {:data 1} :scale (->bigdec 1000000)
            :name "MB" :short "MB" :auto-scale true
            :aliases ["MB" "mb" "megabyte" "megabytes"]}
   :GB     {:dim {:data 1} :scale (->bigdec 1000000000)
            :name "GB" :short "GB" :auto-scale true
            :aliases ["GB" "gb" "gigabyte" "gigabytes"]}
   :TB     {:dim {:data 1} :scale (->bigdec 1000000000000)
            :name "TB" :short "TB" :auto-scale true
            :aliases ["TB" "tb" "terabyte" "terabytes" "terrabyte" "terrabytes"]}
   :PB     {:dim {:data 1} :scale (->bigdec 1000000000000000)
            :name "PB" :short "PB" :auto-scale true
            :aliases ["PB" "pb" "petabyte" "petabytes"]}
   :EB     {:dim {:data 1} :scale (->bigdec 1000000000000000000)
            :name "EB" :short "EB"
            :aliases ["EB" "eb" "exabyte" "exabytes"]}
   ;; Binary (IEC)
   :KiB    {:dim {:data 1} :scale (->bigdec 1024)
            :name "KiB" :short "KiB"
            :aliases ["KiB" "kib" "kibibyte" "kibibytes"]}
   :MiB    {:dim {:data 1} :scale (->bigdec 1048576)
            :name "MiB" :short "MiB"
            :aliases ["MiB" "mib" "mebibyte" "mebibytes"]}
   :GiB    {:dim {:data 1} :scale (->bigdec 1073741824)
            :name "GiB" :short "GiB"
            :aliases ["GiB" "gib" "gibibyte" "gibibytes"]}
   :TiB    {:dim {:data 1} :scale (->bigdec 1099511627776)
            :name "TiB" :short "TiB"
            :aliases ["TiB" "tib" "tebibyte" "tebibytes"]}
   :PiB    {:dim {:data 1} :scale (->bigdec 1125899906842624)
            :name "PiB" :short "PiB"
            :aliases ["PiB" "pib" "pebibyte" "pebibytes"]}
   :EiB    {:dim {:data 1} :scale (->bigdec 1152921504606846976)
            :name "EiB" :short "EiB"
            :aliases ["EiB" "eib" "exbibyte" "exbibytes"]}
   ;; Bits (decimal)
   :Kb     {:dim {:data 1} :scale (->bigdec 125)
            :name "Kb" :short "Kb"
            :aliases ["Kb" "kilobit" "kilobits"]}
   :Mb     {:dim {:data 1} :scale (->bigdec 125000)
            :name "Mb" :short "Mb"
            :aliases ["Mb" "megabit" "megabits"]}
   :Gb     {:dim {:data 1} :scale (->bigdec 125000000)
            :name "Gb" :short "Gb"
            :aliases ["Gb" "gigabit" "gigabits"]}
   :Tb     {:dim {:data 1} :scale (->bigdec 125000000000)
            :name "Tb" :short "Tb"
            :aliases ["Tb" "terabit" "terabits" "terrabit" "terrabits"]}
   :Pb     {:dim {:data 1} :scale (->bigdec 125000000000000)
            :name "Pb" :short "Pb"
            :aliases ["Pb" "petabit" "petabits"]}
   :Eb     {:dim {:data 1} :scale (->bigdec 125000000000000000)
            :name "Eb" :short "Eb"
            :aliases ["Eb" "exabit" "exabits"]}

   ;; ---- Force ----
   :N      {:dim {:mass 1 :length 1 :time -2} :scale (->bigdec 1)
            :name "newtons" :short "N"
            :aliases ["n" "newton" "newtons"]}

   ;; ---- Energy ----
   :J      {:dim {:mass 1 :length 2 :time -2} :scale (->bigdec 1)
            :name "joules" :short "J" :auto-scale true
            :aliases ["j" "joule" "joules"]}
   :cal    {:dim {:mass 1 :length 2 :time -2} :scale (->bigdec 4.184)
            :name "cal" :short "cal" :auto-scale true
            :aliases ["cal" "calorie" "calories"]}
   :kcal   {:dim {:mass 1 :length 2 :time -2} :scale (->bigdec 4184)
            :name "kcal" :short "kcal" :auto-scale true
            :aliases ["kcal" "kilocalorie" "kilocalories"]}
   :kWh    {:dim {:mass 1 :length 2 :time -2} :scale (->bigdec 3600000)
            :name "kWh" :short "kWh" :auto-scale true
            :aliases ["kwh" "kWh" "kilowatt-hour" "kilowatt-hours"]}
   :BTU    {:dim {:mass 1 :length 2 :time -2} :scale (->bigdec 1055.06)
            :name "BTU" :short "BTU" :auto-scale true
            :aliases ["btu" "BTU" "btus"]}
   :eV     {:dim {:mass 1 :length 2 :time -2} :scale (->bigdec 1.602176634E-19)
            :name "eV" :short "eV" :auto-scale true
            :aliases ["ev" "eV" "electronvolt" "electronvolts"]}
   :Wh     {:dim {:mass 1 :length 2 :time -2} :scale (->bigdec 3600)
            :name "Wh" :short "Wh"
            :aliases ["wh" "Wh" "watt-hour" "watt-hours"]}

   ;; ---- Power ----
   :W      {:dim {:mass 1 :length 2 :time -3} :scale (->bigdec 1)
            :name "watts" :short "W" :auto-scale true
            :aliases ["w" "watt" "watts"]}
   :mW     {:dim {:mass 1 :length 2 :time -3} :scale (->bigdec 0.001)
            :name "mW" :short "mW" :auto-scale true
            :aliases ["mw" "milliwatt" "milliwatts"]}
   :kW     {:dim {:mass 1 :length 2 :time -3} :scale (->bigdec 1000)
            :name "kW" :short "kW" :auto-scale true
            :aliases ["kw" "kilowatt" "kilowatts"]}
   :MW     {:dim {:mass 1 :length 2 :time -3} :scale (->bigdec 1000000)
            :name "MW" :short "MW" :auto-scale true
            :aliases ["MW" "megawatt" "megawatts"]}
   :GW     {:dim {:mass 1 :length 2 :time -3} :scale (->bigdec 1000000000)
            :name "GW" :short "GW" :auto-scale true
            :aliases ["GW" "gigawatt" "gigawatts"]}
   :TW     {:dim {:mass 1 :length 2 :time -3} :scale (->bigdec 1000000000000)
            :name "TW" :short "TW" :auto-scale true
            :aliases ["TW" "terawatt" "terawatts"]}

   ;; ---- Pressure ----
   :Pa     {:dim {:mass 1 :length -1 :time -2} :scale (->bigdec 1)
            :name "pascals" :short "Pa"
            :aliases ["pa" "pascal" "pascals"]}
   :psi    {:dim {:mass 1 :length -1 :time -2} :scale (->bigdec 6894.757293168)
            :name "psi" :short "psi"
            :aliases ["psi"]}
   :bar    {:dim {:mass 1 :length -1 :time -2} :scale (->bigdec 100000)
            :name "bar" :short "bar"
            :aliases ["bar" "bars"]}
   :atm    {:dim {:mass 1 :length -1 :time -2} :scale (->bigdec 101325)
            :name "atm" :short "atm"
            :aliases ["atm" "atmosphere" "atmospheres"]}
   :mmHg   {:dim {:mass 1 :length -1 :time -2} :scale (->bigdec 133.322387415)
            :name "mmHg" :short "mmHg"
            :aliases ["mmhg" "mmHg"]}
   :torr   {:dim {:mass 1 :length -1 :time -2} :scale (->bigdec 133.322368421)
            :name "torr" :short "torr"
            :aliases ["torr"]}

   ;; ---- Frequency ----
   :Hz     {:dim {:time -1} :scale (->bigdec 1)
            :name "Hz" :short "Hz"
            :aliases ["hz" "Hz" "hertz"]}
   :kHz    {:dim {:time -1} :scale (->bigdec 1000)
            :name "kHz" :short "kHz"
            :aliases ["khz" "kHz" "kilohertz"]}
   :MHz    {:dim {:time -1} :scale (->bigdec 1000000)
            :name "MHz" :short "MHz"
            :aliases ["mhz" "MHz" "megahertz"]}
   :GHz    {:dim {:time -1} :scale (->bigdec 1000000000)
            :name "GHz" :short "GHz"
            :aliases ["ghz" "GHz" "gigahertz"]}

   ;; ---- Electrical ----
   :V      {:dim {:mass 1 :length 2 :time -3 :current -1} :scale (->bigdec 1)
            :name "volts" :short "V" :auto-scale true
            :aliases ["v" "volt" "volts"]}
   :A      {:dim {:current 1} :scale (->bigdec 1)
            :name "amps" :short "A" :auto-scale true
            :aliases ["amp" "amps" "ampere" "amperes"]}
   :mA     {:dim {:current 1} :scale (->bigdec 0.001)
            :name "mA" :short "mA" :auto-scale true
            :aliases ["ma" "milliamp" "milliamps" "milliampere" "milliamperes"]}
   :ohm    {:dim {:mass 1 :length 2 :time -3 :current -2} :scale (->bigdec 1)
            :name "ohms" :short "Ω"
            :aliases ["ohm" "ohms" "Ω"]}
   :F      {:dim {:current 2 :time 4 :mass -1 :length -2} :scale (->bigdec 1)
            :name "farads" :short "F"
            :aliases ["farad" "farads"]}
   :uF     {:dim {:current 2 :time 4 :mass -1 :length -2} :scale (->bigdec 0.000001)
            :name "μF" :short "μF"
            :aliases ["uf" "μF" "uF" "microfarad" "microfarads"]}
   :nF     {:dim {:current 2 :time 4 :mass -1 :length -2} :scale (->bigdec 0.000000001)
            :name "nF" :short "nF"
            :aliases ["nf" "nF" "nanofarad" "nanofarads"]}
   :pF     {:dim {:current 2 :time 4 :mass -1 :length -2} :scale (->bigdec 0.000000000001)
            :name "pF" :short "pF"
            :aliases ["pf" "pF" "picofarad" "picofarads"]}
   :H      {:dim {:mass 1 :length 2 :time -2 :current -2} :scale (->bigdec 1)
            :name "henries" :short "H"
            :aliases ["henry" "henries" "henrys"]}

   ;; ---- Angle ----
   :rad    {:dim {:angle 1} :scale (->bigdec 1)
            :name "radians" :short "rad"
            :aliases ["rad" "radian" "radians"]}
   :deg    {:dim {:angle 1} :scale (->bigdec 0.01745329251994330)
            :name "degrees" :short "°"
            :aliases ["deg" "degree" "degrees"]}

   ;; ---- Speed ----
   :kn     {:dim {:length 1 :time -1} :scale (->bigdec 0.51444444444444)
            :name "knots" :short "kn"
            :aliases ["knot" "knots" "kn" "kt"]}

   ;; ---- Temperature (special: affine transforms, no dim/scale) ----
   :degF   {:temperature true
            :name "fahrenheit" :short "°F"
            :aliases ["f" "fahrenheit"]}
   :degC   {:temperature true
            :name "celsius" :short "°C"
            :aliases ["c" "celsius"]}
   :K      {:temperature true
            :name "kelvin" :short "K"
            :aliases ["k" "kelvin"]}})

;; ============================================================================
;; Derived lookup tables (all computed from unit-defs)
;; ============================================================================

(def unit-aliases
  "Map from English string to canonical unit keyword. Derived from :aliases."
  (into {}
        (for [[unit-key {:keys [aliases]}] unit-defs
              alias aliases]
          [alias unit-key])))

(def canonical-units
  "Map from keyword aliases to canonical unit keyword.
   E.g. :mile -> :mi, :foot -> :ft. Derived from :aliases."
  (into {}
        (for [[unit-key {:keys [aliases]}] unit-defs
              alias aliases
              :let [kw (keyword alias)]
              :when (not= kw unit-key)]
          [kw unit-key])))

(def temperature-units
  (into #{} (for [[k v] unit-defs :when (:temperature v)] k)))

(def dim-categories
  "Map from dimension map to human-readable category label."
  {{:length 1} "Length"
   {:length 2} "Area"
   {:length 3} "Volume"
   {:mass 1} "Mass"
   {:time 1} "Time"
   {:data 1} "Data"
   {:mass 1 :length 1 :time -2} "Force"
   {:mass 1 :length 2 :time -2} "Energy"
   {:mass 1 :length 2 :time -3} "Power"
   {:mass 1 :length -1 :time -2} "Pressure"
   {:mass 1 :length 2 :time -3 :current -1} "Electrical Potential"
   {:current 1} "Electric Current"
   {:mass 1 :length 2 :time -3 :current -2} "Resistance"
   {:current 2 :time 4 :mass -1 :length -2} "Capacitance"
   {:mass 1 :length 2 :time -2 :current -2} "Inductance"})

(def ^:private auto-scale-units
  "Ordered lists of [unit-key scale] for each dimension with auto-scale units."
  (let [as-entries (for [[k v] unit-defs
                         :when (and (:auto-scale v) (:dim v))]
                     [k v])
        by-dim (group-by (fn [[_ v]] (:dim v)) as-entries)]
    (into {}
          (for [[dim entries] by-dim]
            [dim (->> entries
                      (map (fn [[k v]] [k (:scale v)]))
                      (sort-by (fn [[_ s]] #?(:clj (double s) :cljs s))))]))))

(def ^:private unit-display-names
  (into {} (for [[k v] unit-defs :when (:name v)] [k (:name v)])))

(def ^:private unit-short-names
  (into {} (for [[k v] unit-defs :when (:short v)] [k (:short v)])))

;; ============================================================================
;; Dimension and conversion functions
;; ============================================================================

(defn canonical-unit [u]
  (get canonical-units u u))

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

;; ---------------------------------------------------------------------------
;; Auto-scaling: pick the best unit for a given dimension and SI value
;; ---------------------------------------------------------------------------

(defn- pick-best-unit
  "From a sorted candidate list, pick the largest unit where |value/scale| >= 1."
  [candidates abs-val]
  (or (->> candidates
           reverse
           (filter (fn [[_ s]]
                     (>= #?(:clj (double (safe-div abs-val s))
                            :cljs (/ abs-val s))
                         1.0)))
           first)
      (first candidates)))

(defn- try-compound-unit
  "Try to decompose `dim` into num-dim / denom-dim where both are in
   auto-scale-units. Returns {:value ... :unit-label \"W/day\"} or nil."
  [dim si-value]
  (let [known-dims (keys auto-scale-units)
        candidates
        (for [num-dim   known-dims
              :let [remainder (normalize-map
                               (merge-with - dim num-dim))]
              :when (seq remainder)
              :when (every? neg? (vals remainder))
              :let [denom-dim (normalize-map
                               (into {} (map (fn [[k v]] [k (- v)]) remainder)))]
              :when (get auto-scale-units denom-dim)
              :let [num-cands   (get auto-scale-units num-dim)
                    denom-cands (get auto-scale-units denom-dim)]
              [denom-key denom-scale] denom-cands
              :let [;; value in "num-SI / this-denom-unit" = si-value * denom-scale
                    adjusted (* si-value denom-scale)
                    abs-adj  #?(:clj (.abs (bigdec adjusted)) :cljs (js/Math.abs adjusted))
                    [num-key num-scale] (pick-best-unit num-cands abs-adj)
                    converted (normalize-number (safe-div adjusted num-scale))
                    abs-conv #?(:clj (double (.abs (bigdec converted)))
                                :cljs (js/Math.abs converted))
      ]]
          {:value     converted
           :abs-conv  abs-conv
           :unit-label (str (get unit-short-names num-key (name num-key))
                            "/"
                            (get unit-short-names denom-key (name denom-key)))})]
    ;; Pick the candidate whose value is most naturally readable.
    ;; Prefer values in 1-999; penalize values < 1 or >= 1000.
    ;; Tiebreaker: prefer shorter numeric representation.
    (when (seq candidates)
      (let [score (fn [{:keys [abs-conv unit-label]}]
                    (let [range-penalty
                          (cond
                            (and (>= abs-conv 1.0) (< abs-conv 1000.0)) 0.0
                            (< abs-conv 1.0) (Math/log10 (/ 1.0 abs-conv))
                            :else (Math/log10 (/ abs-conv 999.0)))
                          ;; Tiebreaker: prefer shorter numeric representation
                          label-penalty (* 0.001 (count (str abs-conv)))]
                      (+ range-penalty label-penalty)))
            best (apply min-key score candidates)]
        (dissoc best :abs-conv)))))

(defn- auto-select-unit
  "Given a dimension map and a value in SI base units, pick the best unit
   and return {:value converted-value :unit-label \"days\"}."
  [dim si-value]
  (if-let [candidates (get auto-scale-units dim)]
    (let [abs-val #?(:clj (.abs (bigdec si-value)) :cljs (js/Math.abs si-value))
          [unit-key scale] (pick-best-unit candidates abs-val)
          converted (normalize-number (safe-div si-value scale))]
      {:value converted :unit-label (get unit-display-names unit-key (name unit-key))})
    ;; Try compound unit decomposition (e.g. W/day)
    (or (try-compound-unit dim si-value)
        {:value (normalize-number si-value) :unit-label nil})))

(defn- evaluate-qty-expr-auto
  "Evaluate a quantity expression and auto-select the best output unit."
  [{:keys [terms ops]}]
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
                (map vector ops (rest terms)))]
    (auto-select-unit (:dim result) (:value result))))

(defn convert-request [{:keys [op quantity to] :as request}]
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

    (vector? quantity)
    (convert-mixed quantity to)

    (map? quantity)
    (convert-one quantity to)

    :else
    {:error :invalid-request
     :request request}))
