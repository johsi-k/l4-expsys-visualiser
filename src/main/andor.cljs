(ns andor
  (:require
   [clara.rules :as c :refer [defquery defsession]]
   [transpiledsched :as sc]
   [rum.core :as rum]
   [clojure.string :as string]
   ["cytoscape" :as cyto]
   ["cytoscape-dagre" :as dagre]
   ["react-cytoscapejs" :as CytoscapeComponent]))

; important
(c/clear-ns-productions!)

(defquery notifiability []
  [?isNotifiable <- sc/IsNotifiable])

; remember to match rules ns
(defsession schedsesh 'andor 'transpiledsched)

(-> schedsesh
    (c/fire-rules)
    (c/query notifiability))
;; => ({:?isNotifiable #sched.IsNotifiable{:trace [{:name "sched_1", :type "rule", :args [], :trace [#sched.AmountOfWages{:trace [{:name "wagesFact", :type "rule", :args [], :trace []}], :name "AmountOfWages", :type "pred", :args []}]}], :name "IsNotifiable", :type "pred", :args []}})

(def *state (atom {:sched-sesh (-> schedsesh
                                 (c/fire-rules))}))

(defn format-args [args]
  (if (seq args)
    (str "(" (string/join ", " args) ")")
    ""))

(defn nodes [{:keys [type name args trace]}]
  (cons {:id name :type type :args (format-args args)}
        (mapcat nodes trace)))

(defn edges [{:keys [type name args trace]}]
  ; trace is a list of maps [{:a 1 :b [...]} ...]
  (apply concat
         (map #(vector name (:name %)) trace)
         (map edges trace)))


(.use cyto dagre)
(rum/defc cytocomp < rum/reactive []
  (let [state (rum/react *state)
        sesh (:sched-sesh state)
        res (c/query sesh notifiability)
        trace-list (map :?isNotifiable res)
        nodes (mapcat nodes trace-list)
        edges (mapcat edges trace-list)]
    [:> CytoscapeComponent
     {:style {:width "500px" :height "500px" :backgroundColor "eeeeff"}
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

(rum/defc display < rum/reactive []
  (let [state (rum/react *state)
        sesh (:sched-sesh state)
        res (c/query sesh notifiability)
        pp #(with-out-str (cljs.pprint/pprint %))
        trace-list (map :?isNotifiable res)]
    [:div
     ;; for logging
     ;; [:div "query result: " (pp res)]
     ;; [:br]
     ;; [:div "list of trace map: " (pp trace-list)]
     ;; [:br]
     ;; [:div "nodes: " (pp (map nodes trace-list))]
     ;; [:br]
     ;; [:div "cat nodes: " (pp (mapcat nodes trace-list))]

     (cytocomp)


     ]))


(rum/mount (display) (js/document.getElementById "root"))

(defn init []
  (println "boop"))
