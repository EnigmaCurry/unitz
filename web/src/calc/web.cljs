(ns calc.web
  (:require [reagent.core :as r]
            [reagent.dom.client :as rdom]
            [calc.units :as units]
            [calc.eval :as ev]
            [calc.format :as fmt]
            [calc.parser :as parser]
            [clojure.string :as str]))

(defn format-unit-label
  "Format an exponent-map unit like {:ft 2} as 'ft²'."
  [unit]
  (cond
    (keyword? unit)
    (get units/unit-short-names unit (name unit))

    (map? unit)
    (let [pos (into {} (filter (fn [[_ v]] (pos? v))) unit)
          neg (into {} (filter (fn [[_ v]] (neg? v))) unit)]
      (str (str/join "·" (for [[k v] pos]
                           (let [label (get units/unit-short-names k (name k))]
                             (if (= v 1) label (str label "^" v)))))
           (when (seq neg)
             (str "/" (str/join "·" (for [[k v] neg]
                                     (let [label (get units/unit-short-names k (name k))
                                           exp (- v)]
                                       (if (= exp 1) label (str label "^" exp)))))))))

    :else (str unit)))

(defn format-quantity-label
  "Build a canonical display string like '2 m/s' from a parsed quantity."
  [quantity]
  (cond
    (map? quantity)
    (let [val-str (fmt/format-number (:value quantity) nil)]
      (str val-str " " (format-unit-label (:unit quantity))))

    (vector? quantity)
    (str/join " " (map format-quantity-label quantity))

    :else (str quantity)))

(defn evaluate [input fmt-opts]
  (let [input (str/trim input)]
    (when-not (str/blank? input)
      (try
        ;; First try as pure math expression
        (if-let [math-result (parser/parse-math input)]
          {:result (fmt/format-number math-result fmt-opts)}
          ;; Then try as unit conversion
          (let [parsed (parser/parse-request input)
                effective-fmt (merge (:format parsed) fmt-opts)
                result (ev/convert-request parsed)]
            (cond
              (not (:ok? result))
              {:error (fmt/format-error result)}

              (= :percentage (:op parsed))
              {:result (str (fmt/format-number (:value result) effective-fmt)
                            (:unit-label result))}

              (= :root (:op parsed))
              {:result (fmt/format-number (:value result) effective-fmt)}

              (= :modulo (:op parsed))
              {:result (fmt/format-number (:value result) effective-fmt)}

              ;; Mixed output (e.g., "feet and inches")
              (:mixed result)
              {:from input
               :result (str/join " "
                                 (for [{:keys [value unit-label]} (:mixed result)]
                                   (str (fmt/format-number value effective-fmt) " " unit-label)))}

              (:unit-label result)
              {:from input
               :result (str (fmt/format-number (:value result) effective-fmt) " " (:unit-label result))}

              :else
              (let [display (parser/split-display-parts input)
                    from (or (:from display) (format-quantity-label (:quantity parsed)))
                    target (when (not= :auto (:to parsed))
                             (or (:target display) (format-unit-label (:to parsed))))]
                (if target
                  {:from from
                   :target target
                   :result (fmt/format-number (:value result) effective-fmt)}
                  {:result (fmt/format-number (:value result) effective-fmt)})))))
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

(defn load-fmt-opts []
  (try
    (when-let [raw (.getItem js/localStorage "calc-fmt-opts")]
      (js->clj (js/JSON.parse raw) :keywordize-keys true))
    (catch :default _ nil)))

(defn save-fmt-opts! [opts]
  (try
    (if opts
      (.setItem js/localStorage "calc-fmt-opts"
                (js/JSON.stringify (clj->js opts)))
      (.removeItem js/localStorage "calc-fmt-opts"))
    (catch :default _ nil)))

(defn load-default-fmt-opts []
  (try
    (when-let [raw (.getItem js/localStorage "calc-default-fmt-opts")]
      (js->clj (js/JSON.parse raw) :keywordize-keys true))
    (catch :default _ nil)))

(defn save-default-fmt-opts! [opts]
  (try
    (if (seq opts)
      (.setItem js/localStorage "calc-default-fmt-opts"
                (js/JSON.stringify (clj->js opts)))
      (.removeItem js/localStorage "calc-default-fmt-opts"))
    (catch :default _ nil)))

(defonce state (r/atom {:input ""
                        :result nil
                        :error nil
                        :history (load-history)
                        :fmt-opts (load-fmt-opts)
                        :default-fmt-opts (load-default-fmt-opts)
                        :hist-index -1
                        :saved-input ""
                        :menu-open false
                        :theme (load-theme)
                        :page :calc}))

(defn effective-fmt-opts
  "Merge default settings with session overrides. Session wins."
  []
  (let [{:keys [default-fmt-opts fmt-opts]} @state]
    (merge default-fmt-opts fmt-opts)))

(defonce log-ref (atom nil))
(defonce suppress-menu (atom false))

(defn scroll-log-to-top []
  (when-let [el @log-ref]
    (set! (.-scrollTop el) 0)))

(defn apply-theme! [theme]
  (let [root (.-documentElement js/document)]
    (.setAttribute root "data-theme" theme)))

(defn toggle-theme! []
  (let [new-theme (if (= "dark" (:theme @state)) "light" "dark")]
    (swap! state assoc :theme new-theme)
    (.setItem js/localStorage "calc-theme" new-theme)
    (apply-theme! new-theme)))

(def examples
  ["100GB / 900Mbps"
   "12 feet in yards"
   "2 cups in tablespoons"
   "3 feet in inches"
   "5 feet 11 inches to cm"
   "300 miles / 65 mph in hours and minutes"
   "100.4 weeks in hours and minutes"
   "180cm in feet and inches"
   "10 is what percent of 250?"
   "100 fahrenheit to celsius"
   "60 mph in ft/s"
   "1 GB in MB"
   "3.5 kg to pounds"
   "2 cubic yards to gallons"
   "100 MB / 10 Mbps in seconds"
   "7 inches in feet as a fraction"
   "15% of 50"
   "sqrt(144)"
   "square root of 2"
   "cube root of 27"
   "4th root of 625"
   "2 * sqrt(25)"
   "2 + 2"
   "3 * (4 + 5)"
   "1e9 BTU in kWh"])

(def unit-groups units/unit-groups)

(defn example-chip [text]
  [:button.example
   {:on-click (fn []
                (swap! state assoc :input text)
                (let [ev (evaluate text (effective-fmt-opts))]
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
                  (swap! state assoc :page :calc)
                  (save-history! (:history @state))
                  (js/setTimeout scroll-log-to-top 0)))}
   text])

(def help-example-groups
  [["Unit Conversion"
    [["12 feet in yards" "4 yd"]
     ["5 miles to km" "8.04672 km"]
     ["100 fahrenheit to celsius" "37.7778 \u00b0C"]
     ["how many inches are in 3 feet?" "36 in"]
     ["3.5 kg to pounds" "7.71618 lb"]
     ["10e9 bytes in GB" "10 GB"]]]
   ["Mixed Quantities"
    [["5 feet 11 inches to cm" "180.34 cm"]
     ["1 hour 30 minutes in seconds" "5400 s"]
     ["6 lb 4 oz in grams" "2834.9523 g"]]]
   ["Mixed Output"
    [["180cm in feet and inches" "5 ft 10.866... in"]
     ["90 minutes in hours and minutes" "1 hr 30 min"]
     ["10000 seconds in hours, minutes, and seconds" "2 hr 46 min 40 s"]
     ["200 lb in stone and pounds" "14 st 4 lb"]]]
   ["Area & Volume"
    [["10 square feet in square meters" "0.9290304 m\u00b2"]
     ["2 cubic yards to gallons" "403.948 gal"]
     ["100 sqft in sqm" "9.290304 m\u00b2"]]]
   ["Compound Units"
    [["60 mph in ft/s" "88 ft/s"]
     ["100 MB / 10 Mbps in seconds" "80 s"]
     ["100 GB / 900 Mbps" "14.81 min"]]]
   ["Roots"
    [["sqrt(144)" "12"]
     ["square root of 2" "1.4142..."]
     ["cube root of 27" "3"]
     ["4th root of 625" "5"]
     ["fifth root of 32" "2"]
     ["root(3, 125)" "5"]
     ["2 * sqrt(25)" "10"]]]
   ["Percentages"
    [["15% of 50" "7.5"]
     ["10 is what percent of 100?" "10%"]
     ["what is 25 percent of 200" "50"]]]
   ["Math"
    [["2 + 2" "4"]
     ["3 * (4 + 5)" "27"]
     ["2^10" "1024"]
     ["sqrt(9) + sqrt(16)" "7"]]]
   ["Formatting"
    [["7 inches in feet as a fraction" "7/12 ft"]
     ["square root of 2 rounded to 4 decimals" "1.4142"]
     ["5 miles in km with 3 sig figs" "8.05 km"]]]])

(defn- run-example [input]
  (swap! state assoc :input input)
  (let [ev (evaluate input (effective-fmt-opts))]
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
    (js/setTimeout scroll-log-to-top 0)))

(defn help-page []
  [:div.help-page
   [:div.help-header
    [:button.back-btn
     {:on-click #(swap! state assoc :page :calc)}
     "\u2190 Back"]
    [:h2 "Help"]]
   (let [snapshot-meta (.querySelector js/document "meta[name='calc-snapshot']")
         snapshot-sha (when snapshot-meta (.getAttribute snapshot-meta "content"))
         git-sha (some-> (.querySelector js/document "meta[name='calc-git-sha']")
                         (.getAttribute "content"))
         dev? (= git-sha "dev")
         mode (cond snapshot-sha :snapshot dev? :dev :else :pwa)]
     [:div
      [:p.help-intro
       [:strong "calc"] " is a unit conversion calculator that understands natural English. "
       "It supports dimensional analysis across length, weight, volume, temperature, speed, time, data, and more. "
       (case mode
         :snapshot
         [:<>
          "This is a static snapshot build (" snapshot-sha "). Visit "
          [:a {:href "https://calc.rymcg.tech" :target "_blank" :rel "noopener"} "calc.rymcg.tech"]
          " to download the latest release."]

         :dev
         "This is a development build."

         :pwa
         [:<>
          "This page is a static HTML/JS PWA (Progressive Web App) - all calculations are performed client-side in your browser. "
          "You can install this page from your browser menu to your desktop / home screen and run it offline as an app. "
          "You can also download a single self-contained file you can run from anywhere."])]
      (when (= mode :pwa)
        [:div {:style {:text-align "center" :margin-bottom "1.5rem"}}
         [:a.download-btn {:href "/calc.html" :download "calc.html"} "Download"]])])
   (for [[group-name entries] help-example-groups]
     ^{:key group-name}
     [:div.unit-group
      [:h3 group-name]
      [:div.unit-table {:style {:grid-template-columns "1fr"}}
       (for [[input output] entries]
         ^{:key input}
         [:div.unit-row {:style {:cursor "pointer"}
                         :on-click #(run-example input)}
          [:code {:style {:flex "1"}} input]
          [:span.unit-label {:style {:text-align "right"}} (str "\u2192 " output)]])]])
   [:div.unit-group
    [:h3 "Commands"]
    [:div.unit-table
     [:div.unit-row [:span.unit-sym "/help"] [:span.unit-label "Show this help page"]]
     [:div.unit-row [:span.unit-sym "/p N"] [:span.unit-label "Set precision to N decimals (session)"]]
     [:div.unit-row [:span.unit-sym "/p"] [:span.unit-label "Clear session precision"]]
     [:div.unit-row [:span.unit-sym "/s N"] [:span.unit-label "Set sig-figs to N (session)"]]
     [:div.unit-row [:span.unit-sym "/s"] [:span.unit-label "Clear session sig-figs"]]
     [:div.unit-row [:span.unit-sym "clear"] [:span.unit-label "Clear all history"]]]]
   [:h2 {:style {:margin-top "1.5rem" :margin-bottom "0.5rem" :font-size "1.1rem" :color "var(--accent)"}} "Available Units"]
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

(defn settings-page []
  (let [defaults (or (:default-fmt-opts @state) {})
        has-round (contains? defaults :round)
        has-sigs (contains? defaults :sig-figs)
        round-val (or (:round defaults) 4)
        sigs-val (or (:sig-figs defaults) 6)
        theme (:theme @state)]
    [:div.help-page
     [:div.help-header
      [:button.back-btn
       {:on-click #(swap! state assoc :page :calc)}
       "\u2190 Back"]
      [:h2 "Settings"]]
     [:div.settings-section
      [:h3 "Appearance"]
      [:div.setting-row
       [:label.setting-label
        [:input {:type "checkbox"
                 :checked (= theme "dark")
                 :on-change (fn [_] (toggle-theme!))}]
        "Dark mode"]]]
     [:div.settings-section
      [:h3 "Default Formatting"]
      [:p.group-desc
       "Set default precision for all calculations. "
       "Use /p and /s commands to override per session."]
      [:div.setting-row
       [:label.setting-label
        [:input {:type "checkbox"
                 :checked has-round
                 :on-change
                 (fn [_]
                   (let [new-opts (if has-round
                                   (dissoc defaults :round)
                                   (-> defaults
                                       (dissoc :sig-figs)
                                       (assoc :round round-val)))]
                     (swap! state assoc :default-fmt-opts new-opts)
                     (save-default-fmt-opts! new-opts)))}]
        "Decimal places"]
       (when has-round
         [:input.setting-input
          {:type "number" :min 0 :max 20 :value round-val
           :on-change
           (fn [e]
             (let [n (js/parseInt (.. e -target -value) 10)]
               (when-not (js/isNaN n)
                 (let [new-opts (assoc defaults :round n)]
                   (swap! state assoc :default-fmt-opts new-opts)
                   (save-default-fmt-opts! new-opts)))))}])]
      [:div.setting-row
       [:label.setting-label
        [:input {:type "checkbox"
                 :checked has-sigs
                 :on-change
                 (fn [_]
                   (let [new-opts (if has-sigs
                                   (dissoc defaults :sig-figs)
                                   (-> defaults
                                       (dissoc :round)
                                       (assoc :sig-figs sigs-val)))]
                     (swap! state assoc :default-fmt-opts new-opts)
                     (save-default-fmt-opts! new-opts)))}]
        "Significant figures"]
       (when has-sigs
         [:input.setting-input
          {:type "number" :min 1 :max 20 :value sigs-val
           :on-change
           (fn [e]
             (let [n (js/parseInt (.. e -target -value) 10)]
               (when-not (js/isNaN n)
                 (let [new-opts (assoc defaults :sig-figs n)]
                   (swap! state assoc :default-fmt-opts new-opts)
                   (save-default-fmt-opts! new-opts)))))}])]]]))

(def clear-commands #{"clear" "/clear" "reset" "/reset"})

(defn clear-history! []
  (swap! state assoc :input "" :result nil :error nil :history [] :fmt-opts nil)
  (save-history! []))

(defn delete-history-entry! [idx]
  (swap! state update :history (fn [h] (into [] (concat (subvec h 0 idx) (subvec h (inc idx))))))
  (save-history! (:history @state)))

(defn parse-slash-command
  "Parse a slash command. Returns {:cmd name :arg value} or nil."
  [input]
  (when (str/starts-with? input "/")
    (let [parts (str/split (subs input 1) #"\s+" 2)]
      {:cmd (first parts) :arg (second parts)})))

(defn- handle-slash-command
  "Handle a slash command. Returns a history entry map to display, or nil for clear."
  [{:keys [cmd arg]}]
  (case cmd
    "help"
    (do (swap! state assoc :page :help) {:input "/help" :result "Showing help page"})

    "p"
    (if (str/blank? arg)
      (do (swap! state update :fmt-opts dissoc :round)
          {:input "/p" :result "Precision cleared (session)"})
      (let [n (js/parseInt arg 10)]
        (if (js/isNaN n)
          {:input (str "/p " arg) :error "/p requires a number"}
          (do (swap! state assoc :fmt-opts (-> (or (:fmt-opts @state) {})
                                               (dissoc :sig-figs)
                                               (assoc :round n)))
              {:input (str "/p " n) :result (str "Precision set to " n " decimal places (session)")}))))

    "s"
    (if (str/blank? arg)
      (do (swap! state update :fmt-opts dissoc :sig-figs)
          {:input "/s" :result "Sig-figs cleared (session)"})
      (let [n (js/parseInt arg 10)]
        (if (js/isNaN n)
          {:input (str "/s " arg) :error "/s requires a number"}
          (do (swap! state assoc :fmt-opts (-> (or (:fmt-opts @state) {})
                                               (dissoc :round)
                                               (assoc :sig-figs n)))
              {:input (str "/s " n) :result (str "Sig-figs set to " n " (session)")}))))

    ;; unknown
    {:input (str "/" cmd) :error (str "Unknown command: /" cmd)}))

(defn evaluate! []
  (let [input (parser/clean-phrase (:input @state))]
    (when-not (str/blank? input)
      (cond
        (clear-commands (str/lower-case input))
        (clear-history!)

        (and (str/starts-with? input "/")
             (not (clear-commands (str/lower-case input))))
        (let [parsed (parse-slash-command input)
              entry (handle-slash-command parsed)]
          (swap! state assoc :input "" :result (:result entry) :error (:error entry))
          (swap! state update :history (fn [h] (into [entry] h)))
          (save-history! (:history @state))
          (js/setTimeout scroll-log-to-top 0))

        :else
        (let [ev (evaluate input (effective-fmt-opts))]
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
  (let [key (.-key e)
        {:keys [history hist-index saved-input input]} @state]
    (case key
      "Enter"
      (do (evaluate!)
          (swap! state assoc :hist-index -1 :saved-input ""))

      "ArrowUp"
      (let [max-idx (dec (count history))
            new-idx (min (inc hist-index) max-idx)]
        (when (and (seq history) (not= new-idx hist-index))
          (.preventDefault e)
          (when (= hist-index -1)
            (swap! state assoc :saved-input input))
          (swap! state assoc
                 :hist-index new-idx
                 :input (:input (nth history new-idx)))))

      "ArrowDown"
      (when (>= hist-index 0)
        (.preventDefault e)
        (let [new-idx (dec hist-index)]
          (if (neg? new-idx)
            (swap! state assoc :hist-index -1 :input saved-input)
            (swap! state assoc
                   :hist-index new-idx
                   :input (:input (nth history new-idx))))))

      "Escape"
      (do (.preventDefault e)
          (swap! state assoc :input "" :hist-index -1))

      nil)))

(defn app []
  (let [{:keys [input history menu-open]} @state
        eff-fmt (effective-fmt-opts)
        typing? (not (str/blank? input))
        preview (when (and typing?
                           (not (str/starts-with? (str/trim input) "/")))
                  (evaluate input eff-fmt))]
    [:<>
     [:header
      [:h1 "calc"]
      [:div.input-wrapper
       [:input (cond-> {:type "text"
                        :value input
                        :auto-focus true
                        :on-change #(swap! state assoc :input (.. % -target -value) :hist-index -1)
                        :on-key-down on-keydown}
                 (empty? history) (assoc :placeholder "e.g. 100GB / 900Mbps"))]
       (let [clear-fn (fn [e]
                        (.preventDefault e)
                        (.stopPropagation e)
                        (reset! suppress-menu true)
                        (js/setTimeout #(reset! suppress-menu false) 300)
                        (swap! state assoc :input "" :hist-index -1)
                        (let [input-el (some-> (.-target e) .-parentElement (.querySelector "input"))]
                          (js/setTimeout #(when input-el (.blur input-el)) 100)))]
         [:button {:class (str "clear-input" (when (str/blank? input) " empty"))
                   :on-mouse-down clear-fn
                   :on-touch-start clear-fn
                   :on-click (fn [e] (.stopPropagation e))} "\u00d7"])]
      [:button.menu-btn {:on-click #(when-not @suppress-menu (swap! state update :menu-open not))}
       [:span.hamburger]
       [:span.hamburger]
       [:span.hamburger]]]

     (when menu-open
       [:<>
        [:div.menu-overlay {:on-click #(swap! state assoc :menu-open false)}]
        [:nav.menu
         [:button.menu-item
          {:on-click (fn []
                      (swap! state assoc :page :calc :menu-open false))}
          "Home"]
         [:button.menu-item
          {:on-click (fn []
                      (swap! state assoc :page :help :menu-open false))}
          "Help"]
         [:button.menu-item
          {:on-click (fn []
                      (clear-history!)
                      (swap! state assoc :menu-open false))}
          "Clear History"]
         [:button.menu-item
          {:on-click (fn []
                      (swap! state assoc :page :settings :menu-open false))}
          "Settings"]
         [:a.menu-item
          {:href "https://github.com/EnigmaCurry/calc"
           :target "_blank"
           :rel "noopener"
           :on-click #(swap! state assoc :menu-open false)}
          "Source Code"]
         (when-let [sha (some-> (.querySelector js/document "meta[name='calc-git-sha']")
                                (.getAttribute "content"))]
           (when-not (or (= sha "__GIT_SHA__") (= sha "dev"))
             (let [url (str "https://github.com/EnigmaCurry/calc/commit/" sha)]
               [:a.sha-link {:href url :target "_blank" :rel "noopener"
                              :on-click #(swap! state assoc :menu-open false)}
                (str "#" sha)])))]])

     [:main {:ref #(reset! log-ref %)}
      (when preview
        [:div.preview-bar
         [:span.preview-spacer {:aria-hidden "true"} "calc"]
         [:span.preview-answer
          (cond
            (:error preview)
            [:span.preview-error (:error preview)]

            (:target preview)
            [:span.preview-result (str "= " (:result preview) " " (:target preview))]

            :else
            [:span.preview-result (str "= " (:result preview))])]
         [:button.convert {:on-click evaluate!} "="]])
      (case (:page @state)
        :help [help-page]
        :settings [settings-page]
        [:<>
         (when (seq history)
           [:div.log
            (for [[idx {:keys [input from target result error]}] (map-indexed vector history)]
              ^{:key idx}
              [(if (and (zero? idx) (not typing?)) :div.log-entry.latest :div.log-entry)
               {:on-click (fn []
                            (when input
                              (.writeText js/navigator.clipboard input)
                              (swap! state assoc :input input)
                              (when-let [el (.querySelector js/document ".input-wrapper input")]
                                (.focus el)
                                (js/setTimeout
                                 (fn []
                                   (let [len (count input)]
                                     (.setSelectionRange el len len)))
                                 0))))}               [:span.log-input (or from input)]
               (cond
                 error
                 [:span.log-error (str "\u2192 " error)]

                 target
                 [:span.log-result (str "= " result " " target)]

                 :else
                 [:span.log-result (str "= " result)])
               [:button.log-delete
                {:on-click (fn [e]
                             (.stopPropagation e)
                             (delete-history-entry! idx))}
                "\u00d7"]])])
         [:div.examples
          [:h3 "Try some examples"]
          [:div.chips
           (for [ex examples]
             ^{:key ex} [example-chip ex])]]])]]))

(defonce root (atom nil))

(defn ^:export init []
  (apply-theme! (load-theme))
  (let [el (js/document.getElementById "app")]
    (when-not @root
      (reset! root (rdom/create-root el)))
    (rdom/render @root [app])))
