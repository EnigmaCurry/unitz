(ns calc.web
  (:require [reagent.core :as r]
            [reagent.dom.client :as rdom]
            [calc.core :as core]
            [calc.parser :as parser]
            [clojure.string :as str]))

(defn format-number [x]
  (cond
    (js/Number.isInteger x)
    (str (int x))

    :else
    (let [s (.toPrecision (js/Number x) 10)]
      (if (str/includes? s ".")
        (-> s
            (str/replace #"0+$" "")
            (str/replace #"\.$" ""))
        s))))

(defn format-error [{:keys [error unit phrase]}]
  (case error
    :unknown-unit (str "Unknown unit: \"" unit "\"")
    :unparseable (str "Could not parse: \"" phrase "\"")
    :ambiguous-quantities "Both sides of the conversion have quantities"
    :incompatible-dimensions "Incompatible dimensions"
    :unsupported-operation "Unsupported operation"
    :invalid-request "Invalid request"
    (str "Error: " (pr-str {:error error}))))

(defn- split-input [input]
  "Extract the quantity (from) and target unit parts from the input string."
  (let [patterns [#"(?i)^how many (.+) are in (.+)$"
                  #"(?i)^how many (.+) is (.+)$"
                  #"(?i)^(.+) is how many (.+)$"
                  #"(?i)^how much is (.+) (?:in|to) (.+)$"
                  #"(?i)^what is (.+) (?:in|to) (.+)$"
                  #"(?i)^convert (.+) (?:in|to) (.+)$"]]
    (or (some (fn [pat]
                (when-let [[_ a b] (re-matches pat input)]
                  ;; For "how many X are in Y" patterns, quantity is b, target is a
                  (if (re-find #"(?i)^how many .+ (?:are in|is) " input)
                    {:from b :target a}
                    {:from a :target b})))
              patterns)
        ;; Generic "X in/to Y"
        (when-let [[_ from _ to] (re-matches #"(?i)^(.+) (in|to) (.+)$" input)]
          {:from from :target to}))))

(defn evaluate [input]
  (let [input (str/trim input)]
    (when-not (str/blank? input)
      (try
        ;; First try as pure math expression
        (if-let [math-result (parser/parse-math input)]
          {:result (format-number math-result)}
          ;; Then try as unit conversion
          (let [parsed (parser/parse-request input)
                result (core/convert-request parsed)
                {:keys [from target]} (split-input input)]
            (cond
              (core/error? result)
              {:error (format-error result)}

              (:unit-label result)
              {:from input
               :result (str (format-number (:value result)) " " (:unit-label result))}

              (some? from)
              {:from from
               :target target
               :result (format-number result)}

              :else
              {:result (format-number result)})))
        (catch :default e
          {:error (if-let [data (.-data e)]
                    (format-error (js->clj data :keywordize-keys true))
                    (.-message e))})))))


(defn load-history []
  (try
    (when-let [raw (.getItem js/localStorage "calc-history")]
      (js->clj (js/JSON.parse raw) :keywordize-keys true))
    (catch :default _ [])))

(defn save-history! [history]
  (try
    (.setItem js/localStorage "calc-history"
              (js/JSON.stringify (clj->js history)))
    (catch :default _ nil)))

(defn load-theme []
  (or (try (.getItem js/localStorage "calc-theme") (catch :default _ nil))
      (if (and js/window.matchMedia
               (.-matches (.matchMedia js/window "(prefers-color-scheme: light)")))
        "light"
        "dark")))

(defonce state (r/atom {:input ""
                        :result nil
                        :error nil
                        :history (load-history)
                        :menu-open false
                        :theme (load-theme)}))

(defonce log-ref (atom nil))

(defn apply-theme! [theme]
  (let [root (.-documentElement js/document)]
    (.setAttribute root "data-theme" theme)))

(defn toggle-theme! []
  (let [new-theme (if (= "dark" (:theme @state)) "light" "dark")]
    (swap! state assoc :theme new-theme)
    (.setItem js/localStorage "calc-theme" new-theme)
    (apply-theme! new-theme)))

(def examples
  ["12 feet in yards"
   "how many inches are in 3 feet?"
   "5 feet 11 inches to cm"
   "100 fahrenheit to celsius"
   "60 mph in ft/s"
   "1 GB in MB"
   "3.5 kg to pounds"
   "2 cubic yards to gallons"
   "100 MB / 10 Mbps in seconds"
   "2 + 2"
   "3 * (4 + 5)"])

(defn scroll-log-to-top []
  (when-let [el @log-ref]
    (set! (.-scrollTop el) 0)))

(def clear-commands #{"clear" "/clear" "reset" "/reset"})

(defn clear-history! []
  (swap! state assoc :input "" :result nil :error nil :history [])
  (save-history! []))

(defn evaluate! []
  (let [input (str/trim (:input @state))]
    (when-not (str/blank? input)
      (if (clear-commands (str/lower-case input))
        (clear-history!)
        (let [ev (evaluate input)]
          (swap! state assoc
                 :result (:result ev)
                 :error (:error ev)
                 :input "")
          (swap! state update :history
                 (fn [h]
                   (into [{:input input
                           :from (:from ev)
                           :target (:target ev)
                           :result (:result ev)
                           :error (:error ev)}]
                         h)))
          (save-history! (:history @state))
          (js/setTimeout scroll-log-to-top 0))))))

(defn on-keydown [e]
  (when (= "Enter" (.-key e))
    (evaluate!)))

(defn example-chip [text]
  [:button.example
   {:on-click (fn []
                (swap! state assoc :input text)
                (let [ev (evaluate text)]
                  (swap! state assoc
                         :result (:result ev)
                         :error (:error ev)
                         :input "")
                  (swap! state update :history
                         (fn [h]
                           (into [{:input text
                                   :from (:from ev)
                                   :target (:target ev)
                                   :result (:result ev)
                                   :error (:error ev)}]
                                 h)))
                  (save-history! (:history @state))
                  (js/setTimeout scroll-log-to-top 0)))}
   text])

(defn app []
  (let [{:keys [input history menu-open theme]} @state
        preview (when-not (str/blank? input) (evaluate input))]
    [:<>
     [:header
      [:h1 "calc"]
      [:div.input-wrapper
       [:input {:type "text"
                :value input
                :placeholder "e.g. 100GB / 900Mbps"
                :auto-focus true
                :on-change #(swap! state assoc :input (.. % -target -value))
                :on-key-down on-keydown}]
       (when preview
         [:div.preview-dropdown
          (cond
            (:error preview)
            [:span.preview-error (:error preview)]

            (:target preview)
            [:span.preview-result (str "= " (:result preview) " " (:target preview))]

            :else
            [:span.preview-result (str "= " (:result preview))])
          [:button.convert {:on-click evaluate!} "="]])]
      [:button.menu-btn {:on-click #(swap! state update :menu-open not)}
       [:span.hamburger]
       [:span.hamburger]
       [:span.hamburger]]]

     (when menu-open
       [:<>
        [:div.menu-overlay {:on-click #(swap! state assoc :menu-open false)}]
        [:nav.menu
         [:button.menu-item
          {:on-click (fn []
                      (clear-history!)
                      (swap! state assoc :menu-open false))}
          "Clear Everything"]
         [:button.menu-item
          {:on-click (fn []
                      (toggle-theme!)
                      (swap! state assoc :menu-open false))}
          (if (= theme "dark") "Light Mode" "Dark Mode")]
         [:a.menu-item
          {:href "https://github.com/EnigmaCurry/calc"
           :target "_blank"
           :rel "noopener"
           :on-click #(swap! state assoc :menu-open false)}
          "About"]]])

     [:main {:ref #(reset! log-ref %)}
      (if (seq history)
        [:div.log
         (for [[idx {:keys [input from target result error]}] (map-indexed vector history)]
           ^{:key idx}
           [:div.log-entry
            [:span.log-input (or from input)]
            (cond
              error
              [:span.log-error (str " \u2192 " error)]

              target
              [:span.log-result (str " = " result " " target)]

              :else
              [:span.log-result (str " = " result)])])]
        [:div.examples
         [:h3 "Try some examples"]
         [:div.chips
          (for [ex examples]
            ^{:key ex} [example-chip ex])]])]]))

(defonce root (atom nil))

(defn ^:export init []
  (apply-theme! (load-theme))
  (let [el (js/document.getElementById "app")]
    (when-not @root
      (reset! root (rdom/create-root el)))
    (rdom/render @root [app])))
