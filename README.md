# calc

A calculator and unit conversion utility written in Clojure. Parses
natural English phrases like "how many inches are in 3 feet?" and
evaluates them using exact BigDecimal arithmetic (JVM) or floating
point (ClojureScript).

Runs in three places:

 * **CLI** via Babashka or JVM Clojure
 * **Web** at [calc.rymcg.tech](https://calc.rymcg.tech) (100% client-side)
 * **Library** in your Clojure/ClojureScript applications

## Install

### With Nix (recommended)

Run without installing:

```bash
nix run github:EnigmaCurry/calc -- 12 feet in yards
```

Install to your profile:

```bash
nix profile add github:EnigmaCurry/calc
calc 12 feet in yards
```

### Standalone HTML file

Download [calc.html](https://calc.rymcg.tech/calc.html) — a single
self-contained file with all HTML, CSS, and JavaScript inlined. No
server required; open it directly in any browser, save it to your
desktop, or put it on a USB stick. A download link is also available
from the Help page of the [web app](https://calc.rymcg.tech).

### From source

```bash
git clone https://github.com/EnigmaCurry/calc.git
cd calc
just dev              # Enter nix dev shell (provides babashka, clojure, just)
just calc 12 feet in yards
```

## CLI usage

```bash
just calc 12 feet in yards
# 12 feet = 4 yards
```

### Examples

```
$ just calc 100 fahrenheit to celsius
100 fahrenheit = 37.77777777777778 celsius

$ just calc how many inches are in 3 feet?
3 feet = 36 inches

$ just calc 5 feet 11 inches to cm
5 feet 11 inches = 180.34 cm

$ just calc 60 mph in ft/s
60 mph = 88 ft/s

$ just calc 100 MB / 10 Mbps in seconds
100 MB / 10 Mbps = 80 seconds

$ just calc 7 inches in feet as a fraction
7 inches = 7/12 feet

$ just calc -n 100GB / 900Mbps in days
0.01028806584362

$ just calc 2 + 2
4

$ just calc 3 '*' '(4 + 5)'
27
```

### Listing units

```bash
just calc --list                  # All units
just calc --list --kind length    # Just length units
just calc --list --kind data      # Data units
just calc --list --kind compound  # Compound units (mph, Mbps, etc.)
```

### Formatting options

Natural language suffixes:

```
12 feet in yards rounded to 2 decimals    → 4.00
5 miles in km with 3 sig figs             → 8.05
7 inches in feet as a fraction            → 7/12
```

CLI flags (override natural language format):

```
-p N, --precision N    Round to N decimal places
-s N, --sig-figs N     Round to N significant figures
-n,   --numeric        Output bare number only (requires explicit target unit)
```

## Supported syntax

The parser accepts many English forms:

```
12 feet in yards
12 feet to yards
convert 12 feet to yards
how many yards is 12 feet?
how many yards are in 12 feet?
what is 12 feet in yards?
```

### Quantities

| Form | Example |
|------|---------|
| Integers | `12 feet` |
| Decimals | `3.5 kg` |
| Fractions | `1/2 gallon`, `3 1/2 inches` |
| Number words | `twenty three miles` |
| Articles | `a gallon`, `an inch` |
| Mixed | `5 feet 11 inches` |
| Math expressions | `(2 + 3) miles`, `2^10 bytes` |

### Compound and derived units

```
60 miles per hour in ft/s
2 cubic yards to gallons
10 square feet in square meters
100 MB / 10 Mbps in seconds       # quantity arithmetic
60 mph * 2 hours in miles          # quantity arithmetic
```

### Approximation markers

Prefix with `about`, `roughly`, or `~` to indicate approximate intent:

```
about 100 kg in pounds
~ 5 miles in km
```

## Unit categories

Length, Mass, Time, Temperature, Volume, Area, Data (decimal and
binary), Force, Energy, Power, Pressure, Frequency, Electrical (V, A,
ohm, F, H), Angle, Speed.

Run `just calc --list` for the full registry with all aliases.

## Data units and case sensitivity

Data units are **case-sensitive** to distinguish bytes from bits:

| Unit | Meaning | Example |
|------|---------|---------|
| `B`, `KB`, `MB`, `GB`, `TB` | Bytes (decimal, powers of 1000) | `1 GB = 1000 MB` |
| `KiB`, `MiB`, `GiB`, `TiB` | Bytes (binary/IEC, powers of 1024) | `1 GiB = 1024 MiB` |
| `Kb`, `Mb`, `Gb`, `Tb` | Bits (decimal, powers of 1000) | `1 GB = 8 Gb` |

Compound data rates: `Mbps`, `Gbps`, `KBps`, `MBps`, etc.

All other units are case-insensitive.

## Precision

On the JVM and Babashka, all arithmetic uses exact Clojure ratios or
`BigDecimal` with `DECIMAL128` precision (34 significant digits). This
means conversions between units with exact rational scale factors
(feet/yards, bytes/kilobytes, etc.) produce exact results.

In ClojureScript (web), standard JavaScript floating point is used,
with results rounded to 12 significant digits to suppress noise.

## Library API

Add the dependency, then:

```clojure
(require '[calc.parser :as parser]
         '[calc.eval :as eval])

;; Parse and convert a natural language phrase
(let [request (parser/parse-request "5 feet 11 inches to cm")
      result  (eval/convert-request request)]
  (when (:ok? result)
    (:value result)))
;; => 180.34M

;; Direct scalar conversion
(eval/convert-scalar 12 :ft :yd)
;; => 4N

;; Temperature (affine, not multiplicative)
(eval/convert-temperature 100 :degC :degF)
;; => 212N
```

`convert-request` returns a uniform envelope:

```clojure
;; Success
{:ok? true :value 4N}

;; Success with auto-scaled unit (no target specified)
{:ok? true :value 80 :unit-label "seconds"}

;; Error
{:ok? false :error :incompatible-dimensions :from {:length 1} :to {:mass 1}}
```

## Architecture

| Namespace | Responsibility |
|-----------|----------------|
| `calc.units` | Unit registry, aliases, dimensions, unit algebra |
| `calc.parser` | Natural language text to structured request AST |
| `calc.eval` | Request AST to result (conversion engine) |
| `calc.format` | Numbers and errors to display strings |
| `calc.cli` | CLI argument handling and output |
| `calc.web` | Reagent SPA (ClojureScript) |

## Web app

A live instance is at [calc.rymcg.tech](https://calc.rymcg.tech).

```bash
just web-build        # Build static site (output: web/public/)
just web-dev          # Dev server with hot reload (http://localhost:8080)
```

## Development

```bash
just dev              # Enter nix dev shell
just test             # Run all tests (Babashka + JVM)
just calc --list      # List all supported units
just web-build        # Build the static web app
just web-dev          # Web dev server with hot reload
```
