(ns org.knotation.kn-test
  (:require [clojure.test :refer [deftest is testing]]
            [org.knotation.util :as util]
            [org.knotation.environment :as en]
            [org.knotation.format :as fm]
            [org.knotation.kn :as kn]))

(defn test-roundtrip-line
  [parse-fn read-fn render-fn line]
  (->> line
       parse-fn
       (read-fn nil)
       (render-fn nil)
       fm/render-parses
       (= line)
       is))

(defn test-before-after
  [before after]
  (->> before
       util/split-lines
       (fm/read-lines :kn en/blank-env)
       (fm/render-states :kn en/blank-env)
       fm/render-output
       (= after)
       is))

(defn test-roundtrip
  [content]
  (test-before-after content content))

(deftest test-roundtrip-lines
  (test-roundtrip-line kn/parse-blank kn/read-blank kn/render-blank "\n")
  (test-roundtrip-line kn/parse-comment kn/read-comment kn/render-comment "# Foo \n")
  (test-roundtrip-line kn/parse-prefix kn/read-prefix kn/render-prefix "@prefix foo: <bar>\n")
  (test-roundtrip-line kn/parse-subject kn/read-subject kn/render-subject ": <bar>\n")
  ;(println (->> "<foo>: bar\n" kn/parse-statement (kn/read-statement nil) (kn/render-statement nil)))
  ;(println (->> "<foo>; <bat>: bar\n" kn/parse-statement (kn/read-statement nil)))
  ;(println (->> "<foo>; @en: bar\n" kn/parse-statement (kn/read-statement nil)))
  (test-roundtrip-line kn/parse-statement kn/read-statement kn/render-statement "<foo>: bar\n")
  (test-roundtrip-line kn/parse-statement kn/read-statement kn/render-statement "<foo>; <bat>: bar\n")
  (test-roundtrip-line kn/parse-statement kn/read-statement kn/render-statement "<foo>; @en: bar\n"))

(deftest test-rountrips
  (test-roundtrip
   "@prefix ex: <http://example.com/>

# comment

: ex:s
ex:p; ex:d: Multiline
 string
 
  with spaces.
"))

(deftest test-templates
  (test-before-after
   "@prefix knp: <https://knotation.org/predicate/>
@prefix ex: <http://example.com/>

: ex:template
knp:template-content: 
 ex:label: Foo {label}

: ex:1
ex:foo: bar
knp:apply-template: ex:template
 label: Bar"
   "@prefix knp: <https://knotation.org/predicate/>
@prefix ex: <http://example.com/>

: ex:template
knp:template-content: 
 ex:label: Foo {label}

: ex:1
ex:foo: bar
knp:applied-template: ex:template
 label: Bar
ex:label: Foo Bar
"))
