(ns org.knotation.clj-api-test
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [orchestra.spec.test :as stest]

            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]
            [org.knotation.ttl :as ttl]
            [org.knotation.clj-api :as api]
            [org.knotation.clj-api-spec]))

(stest/instrument)

(def test-1-kn
  "@prefix kn: <https://knotation.org/kn/>
@prefix ex: <http://example.com/>

: ex:s
ex:p; kn:link: ex:o
> ex:a; kn:link: ex:b")

(def test-1-ttl
  "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
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

(def test-2-kn
  "@prefix ex: <http://example.com/>

: ex:s
ex:p: A
> ex:p: B is an annotation on A
>> ex:p: C is an annotation on B
> ex:p: D is an annotation on A")

(def test-2-ttl
  "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix ex: <http://example.com/> .

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
")

(def test-3-ttl
  "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
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
    [
      ex:p ex:x ;
      ex:p ex:y
    ]
    ex:d
  ) ;
  ex:p \"after\" .
")

(defn test-ttl-roundtrip
  [s]
  (->> s
       (api/read-string :ttl {})
       st/sequential-blank-nodes
       (ttl/render-states {})
       (map ::st/output)
       (map ::st/content)
       string/join
       (= s)
       is))

(deftest test-ttl
  (test-ttl-roundtrip test-1-ttl)
  (test-ttl-roundtrip test-2-ttl)
  (test-ttl-roundtrip test-3-ttl))

; TODO: test-kn roundtrips
; TODO: test-kn->ttl
; TODO: test-ttl->kn

#_(->> test-2-ttl
       (api/read-string :ttl {})
       st/sequential-blank-nodes
     ;(org.knotation.util/partition-with #(-> % ::rdf/quad ::rdf/si))
     ;second
     ;(mapcat #(map (partial st/assign-stanza (st/objects-subjects %)) %))
     ;(map ::st/event)
     ;(map ::rdf/quad)
     ;(map ::rdf/zn)
     ;(map (juxt ::rdf/si ::rdf/sb ::rdf/ob))
     ;(map println))
       st/partition-stanzas
     ;(map st/objects-subjects))
       count)

#_(->> test-2-ttl
       (api/read-string :ttl {})
       st/sequential-blank-nodes
       ;(map println))
       (ttl/render-states {})
       (map ::st/output)
       (map ::st/content)
       string/join
       println)
