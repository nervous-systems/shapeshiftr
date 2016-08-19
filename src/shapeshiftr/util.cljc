(ns ^:no-doc shapeshiftr.util
  (:require [clojure.string :as str]
            #? (:clj  [clojure.pprint :as pprint]
                :cljs [cljs.pprint :as pprint]))
  #? (:cljs (:require-macros [shapeshiftr.util])))

#? (:clj
    (defmacro pred-> [v & clauses]
      (let [g       (gensym)
            clauses (mapcat (fn [[pred conseq]] [(list pred g) conseq])
                            (partition 2 clauses))]
        `(let [~g ~v]
           (cond-> ~g ~@clauses)))))

(defn pprint-str [x]
  (str/trimr (with-out-str (pprint/pprint x))))

(defn doc-examples! [vvar examples]
  (alter-meta!
   vvar update :doc str
   "\n\n```clojure\n"
   (str/join
    "\n\n"
    (for [[before after] examples]
      (cond-> (pprint-str before)
        after (str "\n  =>\n" (pprint-str after)))))
   "\n```"))

#? (:clj
    (defmacro with-doc-examples! [vvar & examples]
      `(doc-examples! #'~vvar (quote ~examples))))
