(defproject org.knotation/knotation-cljc "0.1.0-SNAPSHOT"
  :description "A library for working with Knotation format."
  :url "https://github.com/knotation/knotation-cljc"
  :license {:name "BSD 3-Clause License"
            :url "http://opensource.org/licenses/BSD-3-Clause"}
  :dependencies [[org.clojure/clojure "1.9.0-beta3"]]
  :main org.knotation.cli
  :aot [org.knotation.cli])
