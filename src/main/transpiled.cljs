(ns transpiled
  (:require
   [clara.rules :as c :refer [defrule]]))

(defn minsavings [x]
  (* 5000 x))
(defn minincome [x]
  (+ 15000 (* 4000 x)))


(defrecord Savings_account [trace name type args])
(defrecord Income [trace name type args])
(defrecord Earnings [trace name type args])
(defrecord Investment [trace name type args])
(defrecord Amount_saved [trace name type args])
(defrecord Dependents [trace name type args])
(defrecord Member [trace name type args])
(defrecord IsDead [trace name type args])
(defrecord Spendthrift [trace name type args])



(defrule accInad
  [?j0 <- Savings_account [{[j01 j02] :args}]
     (= j01 ?p)
     (= j02 "inadequate")]
  =>
  (c/insert! (->Investment [{:name "accInad" :type "rule" :args [?p] :trace [?j0]}] "Investment" "pred" [?p "savings"])))


(defrule accAdIncAd
  [?j0 <- Savings_account [{[j01 j02] :args}]
     (= j01 ?p)
     (= j02 "adequate")]
  [?j1 <- Income [{[j11 j12] :args}]
     (= j11 ?p)
     (= j12 "adequate")]
  =>
  (c/insert! (->Investment [{:name "accAdIncAd" :type "rule" :args [?p] :trace [?j0 ?j1]}] "Investment" "pred" [?p "stocks"])))


(defrule accAdIncInad
  [?j0 <- Savings_account [{[j01 j02] :args}]
     (= j01 ?p)
     (= j02 "adequate")]
  [?j1 <- Income [{[j11 j12] :args}]
     (= j11 ?p)
     (= j12 "inadequate")]
  =>
  (c/insert! (->Investment [{:name "accAdIncInad" :type "rule" :args [?p] :trace [?j0 ?j1]}] "Investment" "pred" [?p "combination"])))


(defrule savingsAd
  [?j0 <- Amount_saved [{[j01 j02] :args}]
     (= j01 ?p)
     (= j02 ?x)]
  [?j1 <- Dependents [{[j11 j12] :args}]
     (= j11 ?p)
     (= j12 ?y)]
  [:test (> ?x (minsavings ?y))]
  =>
  (c/insert! (->Savings_account [{:name "savingsAd" :type "rule" :args [?p ?x ?y] :trace [?j0 ?j1]}] "Savings_account" "pred" [?p "adequate"])))


(defrule savingsInad
  [?j0 <- Amount_saved [{[j01 j02] :args}]
     (= j01 ?p)
     (= j02 ?x)]
  [?j1 <- Dependents [{[j11 j12] :args}]
     (= j11 ?p)
     (= j12 ?y)]
  [:test (<= ?x (minsavings ?y))]
  =>
  (c/insert! (->Savings_account [{:name "savingsInad" :type "rule" :args [?p ?x ?y] :trace [?j0 ?j1]}] "Savings_account" "pred" [?p "inadequate"])))


(defrule incomeAd
  [?j0 <- Earnings [{[j01 j02 j03] :args}]
     (= j01 ?p)
     (= j02 ?x)
     (= j03 "steady")]
  [?j1 <- Dependents [{[j11 j12] :args}]
     (= j11 ?p)
     (= j12 ?y)]
  [:test (> ?x (minincome ?y))]
  [:not [Spendthrift [{[arg1] :args}]
          (= arg1 ?p)]]
  =>
  (c/insert! (->Income [{:name "incomeAd" :type "rule" :args [?p ?x ?y] :trace [?j0 ?j1]}] "Income" "pred" [?p "adequate"])))


(defrule incomeInadSpendthrift
  [?j0 <- Spendthrift [{[j01] :args}]
     (= j01 ?p)]
  =>
  (c/insert! (->Income [{:name "incomeInadSpendthrift" :type "rule" :args [?p] :trace [?j0]}] "Income" "pred" [?p "inadequate"])))


(defrule incomeInadESteady
  [?j0 <- Earnings [{[j01 j02 j03] :args}]
     (= j01 ?p)
     (= j02 ?x)
     (= j03 "steady")]
  [?j1 <- Dependents [{[j11 j12] :args}]
     (= j11 ?p)
     (= j12 ?y)]
  [:test (<= ?x (minincome ?y))]
  =>
  (c/insert! (->Income [{:name "incomeInadESteady" :type "rule" :args [?p ?x ?y] :trace [?j0 ?j1]}] "Income" "pred" [?p "inadequate"])))


(defrule incomeInadEUnsteady
  [?j0 <- Earnings [{[j01 j02 j03] :args}]
     (= j01 ?p)
     (= j02 ?x)
     (= j03 "unsteady")]
  =>
  (c/insert! (->Income [{:name "incomeInadEUnsteady" :type "rule" :args [?p ?x] :trace [?j0]}] "Income" "pred" [?p "inadequate"])))


(defrule amount_savedAdam

  =>
  (c/insert! (->Amount_saved [{:name "amount_savedAdam" :type "rule" :args [] :trace []}] "Amount_saved" "pred" ["adam" 22000])))


(defrule amount_savedEve

  =>
  (c/insert! (->Amount_saved [{:name "amount_savedEve" :type "rule" :args [] :trace []}] "Amount_saved" "pred" ["eve" 15000])))


(defrule earningsAdam

  =>
  (c/insert! (->Earnings [{:name "earningsAdam" :type "rule" :args [] :trace []}] "Earnings" "pred" ["adam" 25000 "steady"])))


(defrule earningsEve

  =>
  (c/insert! (->Earnings [{:name "earningsEve" :type "rule" :args [] :trace []}] "Earnings" "pred" ["eve" 30000 "unsteady"])))


(defrule dependentsAdam

  =>
  (c/insert! (->Dependents [{:name "dependentsAdam" :type "rule" :args [] :trace []}] "Dependents" "pred" ["adam" 3])))


(defrule dependentsEve

  =>
  (c/insert! (->Dependents [{:name "dependentsEve" :type "rule" :args [] :trace []}] "Dependents" "pred" ["eve" 2])))

