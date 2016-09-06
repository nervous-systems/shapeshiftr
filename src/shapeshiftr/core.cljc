(ns shapeshiftr.core
  "Core functionality for communicating with the <http://shapeshift.io> service.

  This module is designed to be compatible with the JVM, Node, modern browsers.
  In a browser environment, `cors.shapeshift.io` will be used as the target of
  API requests.

  ## Organization

  The API is presented as individual
  functions (e.g. [[validate-address!]], [[shift!]]) which correspond clearly to
  actions in the remote API.  Alternatively, there is a generic [[shapeshift!]]
  function taking as its first argument the keyword operation name.  All
  operations take at most one operation-specific argument (generally a map,
  currency pair vector or number).

  Excepting the first argument, the semantics of [[shapeshift!]] and the
  individual functions are identical.

  ## Representations

  In requests, currency pairs are represented as vectors of keywords, with
  insignificant case.  In responses, pairs are always _uppercase_ keywords.

  shapeshift.io returns some numbers as numeric JSON literals, and others as
  strings.  The default behaviour of Shapeshiftr is to leave the former
  as-is (i.e. as output by the JSON parser), and parse the latter with (JVM)
  `bigdec` and (cljs) `js/parseFloat`.  This behaviour may be undesireable, and
  can be overridden by including a `:str->number` key in
  `opts` (e.g. `{:str->number identity}`).  The supplied function will be
  applied to all string fields which contain numeric values.

  If the string literal \"NaN\" is received where a number is expected, `nil`
  will be substituted in its place.

  ## Responses

  The return value of all functions is
  a (derefable) [Promesa](https://github.com/funcool/promesa) promise, which'll
  resolve to some value (typically a number, boolean, or map), or be rejected
  with an `ExceptionInfo` instance.  The data (`clojure.core/ex-data`)
  associated with the error will contain keys in the namespace `:shapeshiftr/`
  describing the circumstances of the error.

  Lower-level HTTP errors (e.g. timeouts) will come directly
  from [kvlt](https://github.com/nervous-systems/kvlt), and will also take the
  form of `ExceptionInfo` instances."
  (:require [kvlt.core :as kvlt]
            [kvlt.middleware.util :refer [url-encode]]
            [shapeshiftr.munge :as munge]
            [shapeshiftr.util :as util]
            [clojure.string :as str]
            [promesa.core :as p]))

(defn- ->op [s]
  (keyword "shapeshiftr.op" (name (munge/canonical-op s))))

(def ^:private base-url
  (str "https://"
       #? (:cljs (if (not= *target* "nodejs")
                   "cors."))
       "shapeshift.io/"))

(doseq [k #{:rate :limit :marketinfo}]
  (derive (->op k) :shapeshiftr.req/pairwise))

(doseq [k #{:sendamount :shift :mail :cancelpending}]
  (derive (->op k) :shapeshiftr.req/post))

(derive (->op :rate)  :shapeshiftr.resp/numeric)
(derive (->op :limit) :shapeshiftr.resp/numeric)

(defmulti ^:no-doc op->req (fn [req] (-> req :op ->op)))

(defmethod op->req :shapeshiftr.req/pairwise [{:keys [arg op]}]
  {:path (cond-> [(name op)] arg (conj (munge/->pair arg)))})

(defmethod op->req :shapeshiftr.op/validateAddress
  [{{:keys [currency address]} :arg}]
  {:path ["validateAddress" address (str/upper-case (name currency))]})

(defmethod op->req :shapeshiftr.op/txbyaddress [{arg :arg}]
  {:path ["txbyaddress" (arg :address) (arg :api-key)]})

(defmethod op->req :shapeshiftr.req/post [{:keys [arg op]}]
  {:path [(name op)] :method :post :body (munge/tidy-out arg)})

(defmethod ^:no-doc op->req :default [{:keys [arg op]}]
  {:path (cond-> [(name op)] arg (conj arg))})

(defmulti ^:no-doc resp->data :shapeshiftr/op)

(defmethod resp->data :shapeshiftr.op/validateAddress [{body :body}]
  (:isvalid body))

(defmethod resp->data :shapeshiftr.resp/numeric
  [{:keys [shapeshiftr/op body] :as resp}]
  (if-let [v (-> op name keyword body)]
    (munge/*str->number* v)
    (throw (ex-info (str "No value for op " op)
                    {:shapeshiftr/resp  resp
                     :shapeshiftr/error :validation}))))

(defmethod resp->data :default [{body :body}]
  (munge/tidy-in body))

(defmethod resp->data :shapeshiftr.op/sendamount [{body :body}]
  (-> body :success munge/tidy-in))

(defmethod resp->data :shapeshiftr.op/mail [{body :body}]
  (into {}
    (for [[k v] body :let [k (name k)]]
      [k v])))

(defmulti ^:no-doc resp->error :shapeshiftr/op)

(defn- default-error [{body :body :as resp}]
  (let [msg (or (not-empty (body :error)) "<Empty remote error message>")]
    (throw (ex-info msg {:shapeshiftr/resp  resp
                         :shapeshiftr/error :shapeshift}))))

(defmethod resp->error :default [resp]
  (default-error resp))

(defmethod resp->error :shapeshiftr.op/validateAddress [{body :body :as resp}]
  (if (contains? body :isvalid)
    (body :isvalid)
    (default-error resp)))

(defn- adjust-response [{:keys [body shapeshiftr/opts] :as resp}]
  (if (:error body)
    (resp->error resp)
    (binding [munge/*str->number* (get opts :str->number munge/str->number)]
      (resp->data resp))))

(defn- path->url [path]
  (let [path (for [segment path]
               (util/pred-> segment string? url-encode))])
  (apply str base-url (str/join "/" path)))

(defn shapeshift!
  "Send a request to <http://shapeshift.io> (or `cors.shapeshift.io`, in a
  browser environment) for the given `op`(eration) and optional `arg`(ument).

  `op` is a keyword matching one of Shapeshift's API
  operations (e.g. `:marketinfo`, `:txStat`), or a lowercase, hyphenated
  equivalent (`:market-info`, `:tx-stat`).

  `arg`, if present is an `op`-specific value, either a vector (currency pair)
  or map (e.g. transaction description).  This value will be interpolated into
  the URL for `GET` requests, or form the body of a `POST`, depending on the
  operation. "
  [op & [arg opts]]
  (let [op        (->op op)
        req       (op->req {:op op :arg arg})
        url       (path->url (req :path))
        post-keys {:form (req :body) :type :json}
        method    (req :method)]
    (p/then
      (kvlt/request! (cond-> {:url    url
                              :method (or method :get)
                              :as     :json}
                       (= method :post) (merge post-keys)))
      (fn [resp]
        (adjust-response
         (assoc resp
           :shapeshiftr/op   op
           :shapeshiftr/opts opts))))))

(util/with-doc-examples! shapeshift!
  [(shapeshift! :rate [:ltc :btc])
   (Promise= 0.0062122M)]

  [(shapeshift! :rate [:LTC :BTC] {:str->number identity})
   (Promise= "0.0062122")]

  [(shapeshift! :validate-address
                {:currency :btc
                 :address  "38NXHPkXgJvHM2992fssu5PZigVmnPuhR1"})
   (Promise= true)]

  [(shapeshift! :recent-tx 1)
   (Promise= ({:cur-in :LTC :cur-out :XMR :timestamp 1.471691945457E9 :amount 0.18}))])

#? (:clj
    (defn shapeshift!!
      "(Clojure-only) Blocking version of [[shapeshift!]]"
      [op & [arg]]
      @(shapeshift! op arg)))

(defn rate!
  [pair & [opts]]
  (shapeshift! :rate pair opts))

(util/with-doc-examples! rate!
  [(rate! [:ltc :btc])                         (Promise= 0.0062122M)]
  [(rate! [:LTC :BTC] {:str->number identity}) (Promise= "0.0062122")])

(defn limit! [pair & [opts]]
  (shapeshift! :limit pair opts))

(util/with-doc-examples! limit!
  [(limit! [:btc :xmr]) (Promise= 1.63963321M)])

(defn market-info! [& [pair opts]]
  (shapeshift! :market-info pair opts))

(util/with-doc-examples! market-info!
  [(market-info!) (Promise= ({:rate      0.51452586M
                              :limit     109.0626809
                              :pair      [:NMC :NVC]
                              :max-limit 109.0626809
                              :min       0.31428571
                              :miner-fee 0.1} ...))]
  [(market-info [:nmc :nvc]) (Promise= {:rate ... :limit ...})])

(defn recent-tx! [& [max opts]]
  (shapeshift! :recent-tx max opts))

(util/with-doc-examples! recent-tx!
  [(recent-tx! 1)
   (Promise= ({:cur-in    :LTC
               :cur-out   :XMR
               :timestamp 1.471691945457E9
               :amount    0.18}))])

(defn tx-stat! [address & [opts]]
  (shapeshift! :tx-stat address opts))

(util/with-doc-examples! tx-stat!
  [(tx-stat! "1BNELAP1ctmbFqBN4i2jyf3MRRcRBuqU5u")
   (Promise= {:status :no-deposits :address "1BNELAP1ctmbFqBN4i2jyf3MRRcRBuqU5u"})])

(defn time-remaining! [address & [opts]]
  (shapeshift! :time-remaining address opts))

(util/with-doc-examples! time-remaining!
  [(time-remaining! "1BNELAP1ctmbFqBN4i2jyf3MRRcRBuqU5u")
   (Promise= {:status :pending :seconds-remaining 541M})])

(defn coins!
  "Corresponds to `getcoins`."
  [& [opts]]
  (shapeshift! :get-coins nil opts))

(util/with-doc-examples! coins!
  [(coins!) (Promise= {:PPC {:name "Peercoin" :status :available :symbol ...}
                       :BTC ...})])

(defn validate-address! [{:keys [currency address] :as arg} & [opts]]
  (shapeshift! :validate-address arg opts))

(util/with-doc-examples! validate-address!
  [(validate-address!
    {:currency :btc
     :address  "38NXHPkXgJvHM2992fssu5PZigVmnPuhR1"})
   (Promise= true)])

(defn shift!
  "`req` may contain optional keys `:return-address`, `:dest-tag`,`:rs-address`,
  `api-key`."
  [{:keys [withdrawal pair] :as req} & [opts]]
  (shapeshift! :shift req opts))

(util/with-doc-examples! shift!
  [(shift! {:withdrawal "0x2b05b6a4ba45003535512866669a098a69325be4"
            :pair       [:btc :eth]})
   (Promise= {:order-id        "741a5efc-c43e-4c14-8d18-91d424bf9ee2"
              :deposit         "14WqTLUh6CCJwaaEB72ahowNWe5mP3w798"
              :deposit-type    :BTC
              :withdrawal      "0x2b05b6a4ba45003535512866669a098a69325be4"
              :withdrawal-type :ETH
              :api-pub-key     "shapeshift"})])

(defn mail! [{:keys [email txid] :as req} & [opts]]
  (shapeshift! :mail req opts))

(defn send-amount!
  "`req` may additionally contain `:withdrawal`,
  `:returnAddress`, `:dest-tag`, `:rs-address`, `:api-key`.

  Without `:withdrawal`, obtainins a rate for receiving a fixed amount of the
  output currency, without generating a deposit address."
  [{:keys [amount pair] :as req} & [opts]]
  (shapeshift! :sendamount req opts))

(util/with-doc-examples! send-amount!
  [(send-amount! {:amount 1 :pair [:doge :btc]})
   (Promise= {:order-id          "12892346-eff6-4418-a4f2-c7452557ad90"
              :pair              [:DOGE :BTC]
              :withdrawal-amount 1M
              :deposit-amount    2858000M
              :expiration        0
              :quoted-rate       3.5E-7M
              :max-limit         1.13869278068777E7
              :miner-fee         0.0003M})])

(defn cancel-pending! [addr & [opts]]
  (shapeshift! :cancel-pending {:address addr} opts))
