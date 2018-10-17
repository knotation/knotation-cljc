(ns org.knotation.ttl-test
  (:require [clojure.test :refer [deftest is testing]]
            [org.knotation.rdf :as rdf]
            [org.knotation.api :as api]))

(defn test-translation
  [kn ttl]
  (->> kn
       (api/read-from :kn)
       rdf/sequential-blank-nodes
       (api/render-to :ttl)
       (= ttl)
       is))

(deftest test-annotations
  (testing "Basic annotations work"
    (test-translation
     "@prefix kn: <https://knotation.org/kn/>
@prefix ex: <http://example.com/>

: ex:s
ex:p; kn:link: ex:o
> ex:a; kn:link: ex:b"
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

"))
  (testing "Chained annotations work"
    (test-translation
     "@prefix ex: <http://example.com/>

: ex:s
ex:p; kn:link: ex:o
> ex:a; kn:link: ex:b
>> ex:c; kn:link: ex:d"
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

"))
  (testing "Deeply chained annotations work"
    (test-translation
     "@prefix ex: <http://example.com/>

: ex:s
ex:p: A
> ex:p: B is an annotation on A
>> ex:p: C is an annotation on B
> ex:p: D is an annotation on A"
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

")))
