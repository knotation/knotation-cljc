(defproject org.knotation/knotation-cljc "0.1.0-SNAPSHOT"
  :description "A library for working with Knotation format."
  :url "https://github.com/knotation/knotation-cljc"
  :license {:name "BSD 3-Clause License"
            :url "http://opensource.org/licenses/BSD-3-Clause"}
  :plugins [[lein-cljsbuild "1.1.6"]]
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.9.946"]
                 [org.clojure/data.json "0.2.6"]

                 [ring/ring-codec "1.1.0"]

                 [instaparse "1.4.8"]]
  :cljsbuild {:builds [{:source-paths ["src/org/knotation"]
                        :compiler {:output-to "resources/knotation.js"
                                   :optimizations :whitespace
                                   :pretty-print true}
                        :jar true}]}
  :source-paths ["spec" "src"]
  :main org.knotation.cli
  :aot [org.knotation.cli])
