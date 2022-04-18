(ns transpiledsched
  (:require
   [clara.rules :as c :refer [defrule]]))

(defrecord AmountOfWages [trace name type args])
(defrecord Income [trace name type args])
(defrecord IsNotifiable [trace name type args])


(defrule sched_1
  [?j0 <- AmountOfWages [{[] :args}]]
  =>
  (c/insert! (->IsNotifiable [{:name "sched_1" :type "rule" :args [] :trace [?j0]}] "IsNotifiable" "pred" [])))


(defrule sched_2
  [?j0 <- Income [{[] :args}]]
  =>
  (c/insert! (->IsNotifiable [{:name "sched_2" :type "rule" :args [] :trace [?j0]}] "IsNotifiable" "pred" [])))


(defrule wagesFact
  =>
  (c/insert! (->AmountOfWages [{:name "wagesFact" :type "rule" :args [] :trace []}] "AmountOfWages" "pred" [])))


(defrule incomeFact
  =>
  (c/insert! (->Income [{:name "incomeFact" :type "rule" :args [] :trace []}] "Income" "pred" [])))
