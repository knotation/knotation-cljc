(ns org.knotation.kn-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as string]
            [orchestra.spec.test :as stest]

            [org.knotation.util :as util]
            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]
            [org.knotation.state-spec]
            [org.knotation.kn :as kn]
            [org.knotation.kn-spec]
            [org.knotation.api :as api]))

(stest/instrument)

(defn test-roundtrip-line
  [parse-fn read-fn render-fn line]
  (->> line
       parse-fn
       (assoc {::en/env {}} ::st/parse)
       read-fn
       render-fn
       st/render-parse
       (= line)
       is))

(defn normalize-trailing-newlines [s]
  (str (clojure.string/trim s) "\n"))

(defn test-before-after
  [before after]
  (->> before
       (kn/read-input {})
       (kn/render-states {})
       (map ::st/output)
       (map ::st/content)
       string/join
       normalize-trailing-newlines
       (= (normalize-trailing-newlines after))
       is))

(defn test-roundtrip
  [content]
  (test-before-after content content))

(deftest test-roundtrip-lines
  (test-roundtrip-line kn/parse-blank kn/read-blank kn/render-blank "\n")
  (test-roundtrip-line kn/parse-comment kn/read-comment kn/render-comment "# Foo \n")
  (test-roundtrip-line kn/parse-prefix kn/read-prefix kn/render-prefix "@prefix foo: <bar>\n")
  (test-roundtrip-line kn/parse-subject kn/read-subject kn/render-subject ": <bar>\n")
  (for [s ["<foo>: bar\n"
           "<foo>; <bat>: bar\n"
           "<foo>; @en: bar\n"]]
    (test-roundtrip-line kn/parse-statement kn/read-statement (comp kn/render-statement first) s)))

(deftest test-rountrips
  (test-roundtrip "@prefix ex: <http://example.com/>

# comment

: ex:s
ex:p; ex:d: Multiline
 string
 
 with spaces.
"))

(deftest test-templates
  #_(test-before-after
     "@prefix kn: <https://knotation.org/kn/>
@prefix ex: <http://example.com/>

: ex:template
kn:template-content: 
 ex:label: Foo {label}

: ex:1
ex:foo: bar
kn:apply-template: ex:template
 label: Bar"
     "@prefix kn: <https://knotation.org/kn/>
@prefix ex: <http://example.com/>

: ex:template
kn:template-content: 
 ex:label: Foo {label}

: ex:1
ex:foo: bar
kn:applied-template: ex:template
 label: Bar
ex:label: Foo Bar
"))

(deftest test-annotations
  #_(testing "Annotations with no valid targets show up as errors"
      (let [res (->> "@prefix ex: <http://example.com/>

> ex:p: This annotation has no target and should therefore error"
                     util/split-lines
                     (api/read-lines :kn en/blank-env))]
        (is (api/any-errors? res))
        (is (->> (nth res 3) api/error-type (= :no-annotation-target)))))
  #_(testing "Annotations start with pointies"
      (test-roundtrip "@prefix kn: <http://knotation.org/kn/>
@prefix ex: <http://example.com/>

: ex:s
ex:p; kn:link: ex:o
> ex:a; kn:link: ex:b"))
  #_(testing "Annotations can refer to other annotations"
      (let [chained "@prefix kn: <http://knotation.org/kn/>
@prefix ex: <http://example.com/>

: ex:s
ex:p; kn:link: ex:o
> ex:a; kn:link: ex:b
>> ex:c; kn:link: ex:d"
            res (->> chained util/split-lines
                     (api/read-lines :kn en/blank-env))]
        (test-roundtrip chained)
        (is (= #::rdf{:si "http://example.com/s", :pi "http://example.com/p", :ol "ex:o", :di "http://knotation.org/kn/link"}
               (:target (nth res 6))))
        (is (= #::rdf{:si "http://example.com/s", :pi "http://example.com/a", :ol "ex:b", :di "http://knotation.org/kn/link"}
               (:target (nth res 12))))))
  #_(testing "Non-adjacent annotations can refer to previous annotation targets"
      (let [deep-chain "@prefix ex: <http://example.com/>

: ex:s
ex:p: A
> ex:p: B is an annotation on A
>> ex:p: C is an annotation on B
> ex:p: D is an annotation on A"
            res (->> deep-chain util/split-lines
                     (api/read-lines :kn en/blank-env))]
        (test-roundtrip deep-chain)
        (is (= #::rdf{:si "http://example.com/s", :pi "http://example.com/p", :ol "A"}
               (:target (nth res 5))))
        (is (= #::rdf{:si "http://example.com/s", :pi "http://example.com/p", :ol "B is an annotation on A"}
               (:target (nth res 11))))
        (is (= #::rdf{:si "http://example.com/s", :pi "http://example.com/p", :ol "A"}
               (:target (nth res 17))))))
  #_(testing "Annotations can have multi-line strings"
      (test-roundtrip "@prefix ex: <http://example.com/>

: ex:s
ex:p: A
> ex:p: B is an annotation on A
  that includes a multi-line string
>> ex:p: C is an annotation on B
   that likewise includes a multi-line string
> ex:p: D is an annotation on A")))
