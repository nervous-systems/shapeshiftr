(ns shapeshiftr.test.core-test
  (:require [shapeshiftr.core :as shapeshiftr]
            #? (:clj  [clojure.test :refer [deftest is]]
                :cljs [cljs.test :refer-macros [deftest is]])
            [clojure.string :as str]
            [kvlt.core :as kvlt]
            [promesa.core :refer [IPromise]]))

(defn responding [resp]
  ;; Writing asynchronous tests sucks. Doing this is less invasive than
  ;; depending on cats and making the project treat promises as monads.
  (fn [req]
    (reify IPromise
      (-map [_ cb]
        [req (cb resp)]))))

(defn shapeshift!
  ([op response]
   (shapeshift! op nil response))
  ([op arg response]
   (shapeshift! op arg nil response))
  ([op arg opts response]
   (with-redefs [kvlt/request! (responding response)]
     (shapeshiftr/shapeshift! op arg opts))))

(defn path [req]
  (drop 3 (str/split (req :url) #"/")))

(deftest rate
  (let [[req resp] (shapeshift! :rate [:btc :DOGE]
                                {:body {:pair "btc_doge" :rate "7.701"}})]
    (is (= (path req) ["rate" "btc_doge"]))
    (is (= resp #? (:clj 7.701M :cljs 7.701)))))

(deftest rate-no-number
  (let [[_ resp] (shapeshift! :rate [:btc :DOGE] {:str->number identity}
                              {:body {:pair "btc_doge" :rate "0.0013"}})]
    (is (= resp "0.0013"))))

(deftest marketinfo
  (let [[req resp] (shapeshift! :market-info [:doge "ltc"]
                                {:body {:pair     "doge_ltc"
                                        :rate     130.123
                                        :limit    1.23
                                        :min      0.02
                                        :minerFee 0.001}})]
    (is (= (path req) ["marketinfo" "doge_ltc"]))
    (is (= (resp :miner-fee) 0.001))
    (is (= (resp :pair) [:DOGE :LTC]))))

(deftest marketinfo-all
  (let [[req resp] (shapeshift! :market-info {:body [{:pair "doge_ltc"}
                                                     {:pair "btc_ltc"}]})]
    (is (= (path req) ["marketinfo"]))
    (is (= resp [{:pair [:DOGE :LTC]}
                 {:pair [:BTC :LTC]}]))))

(deftest tx-stat
  (let [[req resp] (shapeshift! :tx-stat "xxx"
                                {:body {:status  "no_deposits"
                                        :address "xxx"}})]
    (is (= (path req) ["txStat" "xxx"]))
    (is (= resp {:status :no-deposits :address "xxx"}))))

(deftest tx-stat-complete
  (let [[req resp] (shapeshift! :txStat "xxy"
                                {:body {:status       "complete"
                                        :address      "xxy"
                                        :withdraw     "aab"
                                        :incomingCoin "1.23"
                                        :incomingType "btc"
                                        :outgoingCoin 3.21
                                        :outgoingType "LTC"
                                        :transaction  "123-4"}})]
    (is (= (path req) ["txStat" "xxy"]))
    (is (= (resp :incoming-type) :BTC))
    (is (= (resp :outgoing-type) :LTC))
    (is (= (resp :incoming-coin) #? (:clj 1.23M :cljs 1.23)))
    (is (= (resp :outgoing-coin) 3.21))))
