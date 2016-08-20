(defproject io.nervous/shapeshiftr "0.1.0-SNAPSHOT"
  :description "Clojure/script shapeshift.io (cryptocurrency/altcoin conversion) client"
  :url "https://github.com/nervous-systems/shapeshiftr"
  :license {:name "Unlicense" :url "http://unlicense.org/UNLICENSE"}
  :dependencies [[org.clojure/clojure       "1.8.0"]
                 [org.clojure/clojurescript "1.8.34"]
                 [io.nervous/kvlt           "0.1.3"]
                 [cheshire                  "5.6.3"]
                 [camel-snake-kebab         "0.4.0"]]
  :plugins [[lein-npm       "0.6.0"]
            [lein-codox     "0.9.4"]
            [lein-auto      "0.1.2"]
            [lein-cljsbuild "1.1.1-SNAPSHOT"]
            [lein-doo       "0.1.7-SNAPSHOT"]]
  :codox
  {:source-paths ["src"]
   :metadata     {:doc/format :markdown}
   :html         {:transforms ~(read-string (slurp "doc/assets/codox-transforms.edn"))}
   :source-uri   "https://github.com/nervous-systems/shapeshiftr/blob/master/{filepath}#L{line}"}
  :auto {"codox" {:file-pattern #"\.(clj[cs]?|md)$" :paths ["doc" "src"]}}
  :cljsbuild {:builds
              [{:id "node-test"
                :source-paths ["src" "test"]
                :compiler {:output-to     "target/node-test/test.js"
                           :output-dir    "target/node-test"
                           :target        :nodejs
                           :optimizations :none
                           :main shapeshiftr.test.runner}}
               {:id "node-test-advanced"
                :source-paths ["src" "test"]
                :compiler {:output-to     "target/node-test-adv/test.js"
                           :output-dir    "target/node-test-adv"
                           :target        :nodejs
                           :optimizations :advanced
                           :main          shapeshiftr.test.runner}}
               {:id "generic-test"
                :source-paths ["src" "test"]
                :compiler {:output-to     "target/generic-test/test.js"
                           :optimizations :simple
                           :main          shapeshiftr.test.runner}}]})
