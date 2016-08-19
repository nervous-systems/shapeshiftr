(ns shapeshiftr.core
  "Core functionality for communicating with the <http://shapeshift.io> service"
  (:require [kvlt.core :as kvlt]
            [kvlt.middleware.util :refer [url-encode]]
            [shapeshiftr.munge :as munge]
            [shapeshiftr.util :as util]
            [clojure.string :as str]
            [promesa.core :as p]))

(defn- ->op [s]
  (keyword "shapeshiftr.op" (name s)))

(def ^:private base-url
  (str "https://"
       #? (:cljs (if (not= *target* "nodejs")
                   "cors."))
       "shapeshift.io/"))

(doseq [k #{:rate :limit :marketinfo}]
  (derive (->op k) :shapeshiftr.req/pairwise))

(derive (->op :sendamount) :shapeshiftr.req/post)
(derive (->op :shift)      :shapeshiftr.req/post)

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

(defmethod resp->data :shapeshiftr.op/email [{body :body}]
  (into {}
    (for [[k v] body :let [k (name k)]]
      [k v])))

(defmulti ^:no-doc resp->error :shapeshiftr/op)

(defn- default-error [{body :body :as resp}]
  (throw (ex-info (body :error) {:shapeshiftr/resp  resp
                                 :shapeshiftr/error :shapeshift})))

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
  "Send a request to <http://shapeshift.io> (or `cors.shapeshift.io`, in a browser
  environment) for the given `op`(eration) and optional `arg`(ument).

  `op` is a keyword matching one of Shapeshift's API
  operations (e.g. `:marketinfo`, `:txStat`), or a lowercase, hyphenated
  equivalent (`:market-info`, `:tx-stat`).

  `arg`, if present is an `op`-specific value, either a vector (currency pair)
  or map (e.g. transaction description).  This value will be interpolated into
  the URL for `GET` requests, or form the body of a `POST`, depending on the
  operation.

  The return value is a [Promesa](https://github.com/funcool/promesa) promise,
  which'll resolve to some value (typically a number, boolean, or map), or be
  rejected with an `ExceptionInfo` instance.  The data (`clojure.core/ex-data`)
  associated with the error will contain keys in the namespace `:shapeshiftr/`
  describing the circumstances of the error.

  Lower-level HTTP errors (e.g. timeouts) will come directly
  from [kvlt](https://github.com/nervous-systems/kvlt), and will also take the
  form of `ExceptionInfo` instances.

  shapeshift.io returns some numbers as numeric JSON literals, and others as
  strings.  The default behaviour of Shapeshiftr is to leave the former as-is,
  and parse the latter with (JVM) `bigdec` and (cljs) `js/parseFloat`.  Either
  behaviour may be undesireable, and can be overridden by including a
  `:str->number` key in `opts` (e.g. `{:str->number identity}`).  The supplied
  function will be applied to all fields which appear to be numeric."
  [op & [arg opts]]
  (let [op        (->op (munge/canonical-op op))
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
