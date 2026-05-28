(ns unitz.web
  (:require [reagent.core :as r]
            [reagent.dom.client :as rdom]
            [unitz.core :as core]
            [unitz.parser :as parser]
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

(defn evaluate [input]
  (let [input (str/trim input)]
    (when-not (str/blank? input)
      ;; First try as pure math expression
      (if-let [math-result (parser/parse-math input)]
        {:result (format-number math-result)}
        ;; Then try as unit conversion
        (let [parsed (parser/parse-request input)
              result (core/convert-request parsed)]
          (if (core/error? result)
            {:error (format-error result)}
            {:result (format-number result)}))))))

(defonce state (r/atom {:input ""
                        :result nil
                        :error nil
                        :history []}))

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

(defn evaluate! []
  (let [input (:input @state)]
    (when-not (str/blank? input)
      (let [result (evaluate input)]
        (swap! state assoc
               :result (:result result)
               :error (:error result))
        (when (:result result)
          (swap! state update :history
                 (fn [h]
                   (vec (take-last 20
                                   (conj h {:input input
                                            :result (:result result)}))))))))))

(defn on-keydown [e]
  (when (= "Enter" (.-key e))
    (evaluate!)))

(defn example-chip [text]
  [:button.example
   {:on-click (fn []
                (swap! state assoc :input text)
                (let [result (evaluate text)]
                  (swap! state assoc
                         :result (:result result)
                         :error (:error result))
                  (when (:result result)
                    (swap! state update :history
                           (fn [h]
                             (vec (take-last 20
                                             (conj h {:input text
                                                      :result (:result result)}))))))))}
   text])

(defn app []
  (let [{:keys [input result error history]} @state]
    [:div.container
     [:header
      [:h1 "unitz"]
      [:p.subtitle "Unit conversion & calculator in your browser"]]

     [:main
      [:div.input-group
       [:input {:type "text"
                :value input
                :placeholder "e.g. 12 feet in yards"
                :auto-focus true
                :on-change #(swap! state assoc :input (.. % -target -value))
                :on-key-down on-keydown}]
       [:button.convert {:on-click evaluate!} "Convert"]]

      (when result
        [:div.result
         [:span.label "Result"]
         [:span.value result]])

      (when error
        [:div.error
         [:span.label "Error"]
         [:span.value error]])

      [:div.examples
       [:h3 "Examples"]
       [:div.chips
        (for [ex examples]
          ^{:key ex} [example-chip ex])]]

      (when (seq history)
        [:div.history
         [:h3 "History"]
         [:div.history-list
          (for [{:keys [input result]} (reverse history)]
            ^{:key (str input "-" result)}
            [:div.history-item
             [:span.hist-input input]
             [:span.hist-eq " = "]
             [:span.hist-result result]])]])]

     [:footer
      [:p "Powered by "
       [:a {:href "https://github.com/EnigmaCurry/unitz"
            :target "_blank"
            :rel "noopener"} "unitz"]
       " \u2014 a Clojure/ClojureScript library for unit conversion"]]]))

(defonce root (atom nil))

(defn ^:export init []
  (let [el (js/document.getElementById "app")]
    (when-not @root
      (reset! root (rdom/create-root el)))
    (rdom/render @root [app])))
