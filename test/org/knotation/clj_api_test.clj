(ns org.knotation.clj-api-test
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [orchestra.spec.test :as stest]

            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]
            [org.knotation.ttl :as ttl]
            [org.knotation.clj-api :as api]))

(stest/instrument)

(def test-ttl-string-1
  "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
@prefix owl: <http://www.w3.org/2002/07/owl#>
@prefix kn: <https://knotation.org/kn/> .
@prefix ex: <http://example.com/> .

ex:s
  ex:p ex:o .

_:b0
  rdf:type owl:Annotation ;
  owl:annotatedSource ex:s ;
  owl:annotatedProperty ex:p ;
  owl:annotatedTarget ex:o ;
  ex:a ex:b .")

(def test-ttl-string-2
  "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
@prefix ex: <http://example.com/> .

ex:s
  ex:p \"before\" ;
  ex:p _:b0 ;
  ex:p [
    ex:p \"inner\" ;
    ex:p [
      ex:p \"innermost\"
    ]
  ] ;
  ex:p (
    ex:a
    ex:b 
    ex:c
    ex:d
  ) ;
  ex:p (
    ex:a
    (
      ex:b
      ex:c
    )
    ex:d
  ) ;
  ex:p \"after\" .
")

(deftest test-ttl)

(comment
  (->> test-ttl-string-2
       (api/read-string :ttl {})
       st/sequential-blank-nodes
       ;(map println))
       (ttl/render-states {})
       (map ::st/output)
       (map ::st/content)
       string/join
       println))

(defn test-translation
  [kn ttl]
  ; TODO fix
  #_(->> kn
         (api/read-from :kn)
         rdf/sequential-blank-nodes
         (api/render-to :ttl)
         (= ttl)
         is))

(deftest test-annotations
  (testing "Basic annotations work"
    #_(test-translation
       "@prefix kn: <https://knotation.org/kn/>
@prefix ex: <http://example.com/>

: ex:s
ex:p; kn:link: ex:o
> ex:a; kn:link: ex:b")
    "@prefix kn: <https://knotation.org/kn/> .
@prefix ex: <http://example.com/> .

ex:s
  ex:p ex:o .

_:b0
  rdf:type owl:Annotation ;
  owl:annotatedSource ex:s ;
  owl:annotatedProperty ex:p ;
  owl:annotatedTarget ex:o ;
  ex:a ex:b .

")
  (testing "Chained annotations work"
    #_(test-translation
       "@prefix ex: <http://example.com/>

: ex:s
ex:p; kn:link: ex:o
> ex:a; kn:link: ex:b
>> ex:c; kn:link: ex:d")
    "@prefix ex: <http://example.com/> .

ex:s
  ex:p ex:o .

_:b0
  rdf:type owl:Annotation ;
  owl:annotatedSource ex:s ;
  owl:annotatedProperty ex:p ;
  owl:annotatedTarget ex:o ;
  ex:a ex:b .

_:b1
  rdf:type owl:Annotation ;
  owl:annotatedSource _:b0 ;
  owl:annotatedProperty ex:a ;
  owl:annotatedTarget ex:b ;
  ex:c ex:d .

")
  (testing "Deeply chained annotations work"
    #_(test-translation
       "@prefix ex: <http://example.com/>

: ex:s
ex:p: A
> ex:p: B is an annotation on A
>> ex:p: C is an annotation on B
> ex:p: D is an annotation on A")
    "@prefix ex: <http://example.com/> .

ex:s
  ex:p \"A\" .

_:b0
  rdf:type owl:Annotation ;
  owl:annotatedSource ex:s ;
  owl:annotatedProperty ex:p ;
  owl:annotatedTarget \"A\" ;
  ex:p \"B is an annotation on A\" .

_:b1
  rdf:type owl:Annotation ;
  owl:annotatedSource _:b0 ;
  owl:annotatedProperty ex:p ;
  owl:annotatedTarget \"B is an annotation on A\" ;
  ex:p \"C is an annotation on B\" .

_:b2
  rdf:type owl:Annotation ;
  owl:annotatedSource ex:s ;
  owl:annotatedProperty ex:p ;
  owl:annotatedTarget \"A\" ;
  ex:p \"D is an annotation on A\" .

"))
