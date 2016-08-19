(ns ^:no-doc shapeshiftr.munge
  (:require [clojure.string :as str]
            [shapeshiftr.util :as u]
            [clojure.walk :as walk]
            [camel-snake-kebab.extras :as csk.extras]
            [camel-snake-kebab.core :as csk]))

(defn str->number [s]
  #? (:clj  (bigdec s)
      :cljs (js/parseFloat s)))

(def ^:dynamic *str->number* str->number)

(defn ->currency [x]
  (keyword (str/upper-case x)))

(let [no-camel #{:market-info :recent-tx :time-remaining :get-coins :send-amount
                 :cancel-pending :tx-by-address :tx-by-api-key}]
  (defn canonical-op [op]
    (if (no-camel op)
      (keyword (str/replace (name op) #"-" ""))
      (csk/->camelCaseKeyword op))))

(defn ->pair [x]
  (if (string? x)
    (let [[l r] (str/split x #"_")]
      [(->currency l) (->currency r)])
    (let [l (str/lower-case (name (first x)))
          r (str/lower-case (name (second x)))]
      (str l "_" r))))

(defn number-like? [s]
  (and (string? s) (re-find #"^-?\d+(?:\.\d+)?$" s)))

(defn currency-like? [v]
  (when (or (string? v) (keyword v))
    (re-find #"^[A-Z]{3,6}$" (name v))))

(defn- key-in [k]
  (cond-> k (not (currency-like? k)) csk/->kebab-case))

(def ^:private currency-key?
  #{:curIn :curOut :symbol :inputCurrency :outputCurrency :depositType
    :withdrawalType :incomingType :outgoingType})

(defn tidy-out [m]
  (u/pred-> (csk.extras/transform-keys csk/->camelCase m)
    :pair (update :pair ->pair)))

(defn- tidy-in* [m]
  (into {}
    (for [[k v] m :let [k (keyword k)]]
      [(key-in k) (cond (= k :pair)       (->pair v)
                        (= k :status)     (csk/->kebab-case-keyword v)
                        (currency-key? k) (->currency v)
                        (number-like? v)  (*str->number* v)
                        :else             v)])))

(defn tidy-in [x]
  (walk/postwalk #(u/pred-> % map? tidy-in*) x))
