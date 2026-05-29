(ns calc.web
  (:require [reagent.core :as r]
            [reagent.dom.client :as rdom]
            [calc.units :as units]
            [calc.eval :as ev]
            [calc.format :as fmt]
            [calc.parser :as parser]
            [clojure.string :as str]))

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
            (if-not (:ok? result)
              {:error (fmt/format-error result)}
              (if (:unit-label result)
                {:from input
                 :result (str (fmt/format-number (:value result) effective-fmt) " " (:unit-label result))}
                (let [[display-input] (parser/extract-format input)
                      {:keys [from target]} (parser/split-display-parts display-input)]
                  (if (some? from)
                    {:from from
                     :target target
                     :result (fmt/format-number (:value result) effective-fmt)}
                    {:result (fmt/format-number (:value result) effective-fmt)}))))))
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

(defonce state (r/atom {:input ""
                        :result nil
                        :error nil
                        :history (load-history)
                        :fmt-opts (load-fmt-opts)
                        :hist-index -1
                        :saved-input ""
                        :menu-open false
                        :theme (load-theme)
                        :page :calc}))

(defonce log-ref (atom nil))

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
   "how many inches are in 3 feet?"
   "5 feet 11 inches to cm"
   "100 fahrenheit to celsius"
   "60 mph in ft/s"
   "1 GB in MB"
   "3.5 kg to pounds"
   "2 cubic yards to gallons"
   "100 MB / 10 Mbps in seconds"
   "7 inches in feet as a fraction"
   "2 + 2"
   "3 * (4 + 5)"])

(def unit-groups units/unit-groups)

(defn example-chip [text]
  [:button.example
   {:on-click (fn []
                (swap! state assoc :input text)
                (let [ev (evaluate text (:fmt-opts @state))]
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
          [:span.unit-label label]])]])
   [:div.examples
    [:h3 "Try some examples"]
    [:div.chips
     (for [ex examples]
       ^{:key ex} [example-chip ex])]]])

(def clear-commands #{"clear" "/clear" "reset" "/reset"})

(defn clear-history! []
  (swap! state assoc :input "" :result nil :error nil :history [] :fmt-opts nil)
  (save-history! [])
  (save-fmt-opts! nil))

(defn- parse-slash-command
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
          (save-fmt-opts! (:fmt-opts @state))
          {:input "/p" :result "Precision cleared"})
      (let [n (js/parseInt arg 10)]
        (if (js/isNaN n)
          {:input (str "/p " arg) :error "/p requires a number"}
          (do (swap! state assoc :fmt-opts (-> (or (:fmt-opts @state) {})
                                               (dissoc :sig-figs)
                                               (assoc :round n)))
              (save-fmt-opts! (:fmt-opts @state))
              {:input (str "/p " n) :result (str "Precision set to " n " decimal places")}))))

    "s"
    (if (str/blank? arg)
      (do (swap! state update :fmt-opts dissoc :sig-figs)
          (save-fmt-opts! (:fmt-opts @state))
          {:input "/s" :result "Sig-figs cleared"})
      (let [n (js/parseInt arg 10)]
        (if (js/isNaN n)
          {:input (str "/s " arg) :error "/s requires a number"}
          (do (swap! state assoc :fmt-opts (-> (or (:fmt-opts @state) {})
                                               (dissoc :round)
                                               (assoc :sig-figs n)))
              (save-fmt-opts! (:fmt-opts @state))
              {:input (str "/s " n) :result (str "Sig-figs set to " n)}))))

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
        (let [ev (evaluate input (:fmt-opts @state))]
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
  (let [{:keys [input history menu-open theme fmt-opts]} @state
        typing? (not (str/blank? input))
        preview (when (and typing?
                           (not (str/starts-with? (str/trim input) "/")))
                  (evaluate input fmt-opts))]
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
       [:button {:class (str "clear-input" (when (str/blank? input) " empty"))
                 :on-mouse-down (fn [e]
                                  (.preventDefault e)
                                  (swap! state assoc :input "" :hist-index -1))
                 :on-touch-start (fn [e]
                                   (.preventDefault e)
                                   (swap! state assoc :input "" :hist-index -1))} "\u00d7"]]
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
                      (swap! state assoc :page :calc :menu-open false))}
          "History"]
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
      (if (= :help (:page @state))
        [help-page]
        (if (seq history)
          [:div.log
           (for [[idx {:keys [input from target result error]}] (map-indexed vector history)]
             ^{:key idx}
             [(if (and (zero? idx) (not typing?)) :div.log-entry.latest :div.log-entry)
              {:on-click (fn []
                           (when input
                             (.writeText js/navigator.clipboard input)
                             (swap! state assoc :input input)))}
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
