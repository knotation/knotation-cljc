(defproject org.knotation/knotation-cljc "0.4.0-SNAPSHOT"
  :clojurescript? true
  :description "A library for working with Knotation format."
  :url "https://github.com/knotation/knotation-cljc"
  :license {:name "BSD 3-Clause License"
            :url "http://opensource.org/licenses/BSD-3-Clause"}
  :plugins [[lein-cljsbuild "1.1.6"]
            [lein-doo "0.1.10"]]
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.9.946"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/tools.cli "0.3.5"]

                 [org.apache.jena/jena-arq "3.6.0"]
                 [org.slf4j/slf4j-nop "1.7.12"]

                 [instaparse "1.4.9"]
                 [orchestra "2018.09.10-1"]]
  :cljsbuild {:builds {:main {:source-paths ["src/org/knotation/"]
                              :jar true
                              :compiler {:output-to "resources/knotation.js"
                                         :optimizations :whitespace
                                         :pretty-print true}}
                       :test {:id "test"
                              :source-paths ["src/org/knotation" "test/org/knotation/runner.cljs" "test/org/knotation/cljs_api_test.cljs"]
                              :compiler {:asset-path "resources"}
                                    :output-to "resources/knotation-testable.js"
                                    :output-dir "resources"
                                    :main org.knotation.runner
                                    :optimizations :none}}}
  :source-paths ["src" "spec"]
  :main org.knotation.cli
  :aot [org.knotation.cli])
