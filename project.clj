(defproject knotation/knotation-cljc "0.1.0-SNAPSHOT"
  :description "A library for working with Knotation format."
  :url "https://github.com/knotation/knotation-cljc"
  :license {:name "BSD 3-Clause License"
            :url "http://opensource.org/licenses/BSD-3-Clause"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha17"]
                 [org.clojure/clojurescript "1.9.908"]]
  :plugins [[lein-cljsbuild "1.1.7"]]
  :hooks [leiningen.cljsbuild]
  :cljsbuild {:builds []})
