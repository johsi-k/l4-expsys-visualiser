(ns demo
  (:require
   [clara.rules :as c :refer [defquery defsession]]
   [transpiled :as tp]
   [rum.core :as rum]
   [clojure.string :as string]
   ["cytoscape" :as cyto]
   ["cytoscape-dagre" :as dagre]
   ["react-cytoscapejs" :as CytoscapeComponent]
   ))

; important
; clears any cached rule/query defs from ns
(c/clear-ns-productions!)

(defquery result []
  [?investment <- tp/Investment])

(defsession big-session 'demo 'transpiled)

(-> big-session
    (c/fire-rules)
    (c/query result))

; sanity check
;; (-> big-session
;;     (c/insert (->AmountSaved "pred" "Amount_saved" ["adam" 22000] [{:name "amount_savedAdam" :type "rule" :args [] :trace []}]))
;;     (c/insert (->Earnings "pred" "Earnings" ["adam" 25000 "steady"] [{:name "earningsAdam" :type "rule" :args [] :trace []}]))
;;     (c/insert (->Dependents "pred" "Dependents" ["adam" 2] [{:name "dependentsAdam" :type "rule" :args [] :trace []}]))
;;     (c/fire-rules)
;;     (c/query result))
;; ({:?investment
;;   {:type "pred",
;;    :name "Investment",
;;    :args ["adam" "stocks"],
;;    :trace
;;    [{:type "rule",
;;      :name "accAdIncAd",
;;      :args ["adam"],
;;      :trace
;;      [{:type "pred",
;;        :name "Savings_account",
;;        :args ["adam" "adequate"],
;;        :trace
;;        [{:type "rule",
;;          :name "savingsAd",
;;          :args ["adam" 22000 2],
;;          :trace
;;          [{:type "pred",
;;            :name "Amount_saved",
;;            :args ["adam" 22000],
;;            :trace
;;            [{:name "amount_savedAdam",
;;              :type "rule",
;;              :args [],
;;              :trace []}]}
;;           {:type "pred",
;;            :name "Dependents",
;;            :args ["adam" 2],
;;            :trace
;;            [{:name "dependentsAdam",
;;              :type "rule",
;;              :args [],
;;              :trace []}]}]}]}
;;       {:type "pred",
;;        :name "Income",
;;        :args ["adam" "adequate"],
;;        :trace
;;        [{:type "rule",
;;          :name "incomeAd",
;;          :args ["adam" 25000 2],
;;          :trace
;;          [{:type "pred",
;;            :name "Earnings",
;;            :args ["adam" 25000 "steady"],
;;            :trace [{:name "earningsAdam", :type "rule", :args [], :trace []}]}
;;           {:type "pred",
;;            :name "Dependents",
;;            :args ["adam" 2],
;;            :trace [{:name "dependentsAdam", :type "rule", :args [], :trace []}]}]}]}]}]}})

(def *state (atom {:big-sesh (-> big-session
                                 (c/fire-rules))}))

(defn mk-label [type name args]
  (if (= type "pred")
    (str name " \n(" (string/join ", " args) ")")
    name))

(defn nodes [{:keys [type name args trace]}]
  (cons {:id name :type type :args (str "(" (string/join ", " args) ")")}
        (mapcat nodes trace)))

(defn edges [{:keys [type name args trace]}]
  ; trace is a list of maps [{:a 1 :b [...]} ...]
  (apply concat
         (map #(vector name (:name %)) trace)
         (map edges trace)))


(.use cyto dagre)
(rum/defc cytocomp < rum/reactive []
  (let [state (rum/react *state)
        sesh (:big-sesh state)
        res (c/query sesh result)
        adam (filter (fn [{{args :args} :?investment}]
                       (some #(= "adam" %) args)) res)
        inv (:?investment (first adam) "")
        nodes (nodes inv)
        edges (edges inv)]
    [:> CytoscapeComponent
     {:style {:width "700px" :height "600px" :backgroundColor "eeeeff"}
      :stylesheet [#js {:selector "edge"
                        :style #js {:curve-style "straight"
                                    :source-endpoint "outside-to-node"
                                    :target-endpoint "outside-to-node"}}
                   #js {:selector "node"
                        :style #js {:background-opacity 0.5
                                    :text-valign "center"
                                    :text-wrap "wrap"
                                    :width (fn [ele] (* 10 (max
                                                            (.-length (.data ele "id"))
                                                            (.-length (.data ele "args")))))
                                    }}
                   #js {:selector "[type = 'pred']"
                        :style #js {:background-color "green"
                                    :shape "round-diamond"
                                    :label (fn [ele] (str (.data ele "id") "\n" (.data ele "args")))
                                    }}

                   #js {:selector "[type = 'rule']"
                        :style #js {:background-color "pink"
                                    :label "data(id)"
                                    }}]
      :layout {:name "dagre"}
      :elements (clj->js
                 (concat
                  (for [n nodes]
                    {:data n})
                  (for [[s t] edges]
                    {:data {:id (str s "->" t)
                            :source s
                            :target t}}))
                 )}]))

(comment
  (concat
   (for [n (:nodes g)]
     {:data n})
   (for [[s t] (:edges g)]
     {:data {:id (str s "->" t)
             :source s
             :target t}}))
  ;; => ({:data {:id "Investment", :type "pred", :args "(eve, stocks)"}}
  ;;     {:data {:id "accAdIncAd", :type "rule", :args "(eve)"}}
  ;;     {:data {:id "Savings_account", :type "pred", :args "(eve, adequate)"}}
  ;;     {:data {:id "Income", :type "pred", :args "(eve, adequate)"}}
  ;;     {:data
  ;;      {:id "Investment->accAdIncAd", :source "Investment", :target "accAdIncAd"}}
  ;;     {:data
  ;;      {:id "accAdIncAd->Savings_account",
  ;;       :source "accAdIncAd",
  ;;       :target "Savings_account"}}
  ;;     {:data {:id "accAdIncAd->Income", :source "accAdIncAd", :target "Income"}})

)

(rum/defc display < rum/reactive []
  (let [state (rum/react *state)
        sesh (:big-sesh state)
        res (c/query sesh result)
        adam (filter (fn [{{args :args} :?investment}]
                       (some #(= "adam" %) args)) res)
        ;; inv (:?investment (first res) "")
        inv (:?investment (first adam) "")
        args (:args inv)]
    [:div
     ;; for logging
     ;; [:div "Person: " (first args)]
     ;; [:div "Investment Strategy: " (second args)]
     ;; [:div "ALL RESULTS: " (with-out-str (cljs.pprint/pprint res))]

     ;; [:div "Trace: " (with-out-str (cljs.pprint/pprint inv))]

     ;; [:div "nodes: " (str (nodes inv))]
     ;; [:div "edges: " (str (edges inv))]

     ;; [:div "concat" (str (concat
     ;;              (for [n (nodes inv)]
     ;;                {:data n})
     ;;              (for [[s t] (edges inv)]
     ;;                {:data {:id (str s "->" t)
     ;;                        :source s
     ;;                        :target t}})))]

     (cytocomp)

     ]))


(rum/mount (display) (js/document.getElementById "root"))

(defn init []
  (println "boop"))
