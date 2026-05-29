(ns calc.web
  (:require [reagent.core :as r]
            [reagent.dom.client :as rdom]
            [calc.core :as core]
            [calc.format :as fmt]
            [calc.parser :as parser]
            [clojure.string :as str]))

(defn evaluate [input]
  (let [input (str/trim input)]
    (when-not (str/blank? input)
      (try
        ;; First try as pure math expression
        (if-let [math-result (parser/parse-math input)]
          {:result (fmt/format-number math-result)}
          ;; Then try as unit conversion
          (let [parsed (parser/parse-request input)
                fmt (:format parsed)
                result (core/convert-request parsed)
                [display-input] (parser/extract-format input)
                {:keys [from target]} (parser/split-display-parts display-input)]
            (cond
              (core/error? result)
              {:error (fmt/format-error result)}

              (:unit-label result)
              {:from input
               :result (str (fmt/format-number (:value result) fmt) " " (:unit-label result))}

              (some? from)
              {:from from
               :target target
               :result (fmt/format-number result fmt)}

              :else
              {:result (fmt/format-number result fmt)})))
        (catch :default e
          {:error (if-let [data (.-data e)]
                    (fmt/format-error (js->clj data :keywordize-keys true))
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
                        :theme (load-theme)
                        :page :calc}))

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

(def unit-groups core/unit-groups)

(defn help-page []
  [:div.help-page
   [:div.help-header
    [:button.back-btn
     {:on-click #(swap! state assoc :page :calc)}
     "\u2190 Back"]
    [:h2 "Available Units"]]
   [:p.help-intro
    "All conversions run 100% client-side in your browser \u2014 nothing is sent to a server. "
    "Type natural English phrases like "
    [:code "5 feet in meters"]
    " or "
    [:code "100 GB / 10 Mbps in minutes"]
    ". You can also use compound units with "
    [:code "/"]
    " and "
    [:code "*"]
    " operators."]
   (for [{group-name :name :keys [description units]} unit-groups]
     ^{:key group-name}
     [:div.unit-group
      [:h3 group-name]
      [:p.group-desc description]
      [:div.unit-table
       (for [[sym label] units]
         ^{:key sym}
         [:div.unit-row
          [:span.unit-sym (name sym)]
          [:span.unit-label label]])]])])

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
          (swap! state assoc :page :calc)
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
         [:button.menu-item
          {:on-click (fn []
                      (swap! state assoc :page :help :menu-open false))}
          "Help"]
         [:a.menu-item
          {:href "https://github.com/EnigmaCurry/calc"
           :target "_blank"
           :rel "noopener"
           :on-click #(swap! state assoc :menu-open false)}
          "GitHub"]]])

     [:main {:ref #(reset! log-ref %)}
      (if (= :help (:page @state))
        [help-page]
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
              ^{:key ex} [example-chip ex])]]))]]))

(defonce root (atom nil))

(defn ^:export init []
  (apply-theme! (load-theme))
  (let [el (js/document.getElementById "app")]
    (when-not @root
      (reset! root (rdom/create-root el)))
    (rdom/render @root [app])))
