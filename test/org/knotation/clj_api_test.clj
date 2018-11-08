(ns org.knotation.clj-api-test
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [orchestra.spec.test :as stest]

            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]
            [org.knotation.ttl :as ttl]
            [org.knotation.kn :as kn]
            [org.knotation.clj-api :as api]
            [org.knotation.clj-api-spec]))

(stest/instrument)

(def test-0-kn
  "@prefix kn: <https://knotation.org/kn/>
@prefix ex: <http://example.com/>

# comment

: ex:s
ex:p; kn:link: ex:o
ex:p: o
ex:p; ex:d: o
ex:p; @en: o")

(def test-0-ttl
  "@prefix kn: <https://knotation.org/kn/> .
@prefix ex: <http://example.com/> .

ex:s
  ex:p ex:o ;
  ex:p \"o\" ;
  ex:p \"o\"^^ex:d ;
  ex:p \"o\"@en .
")

(def test-A-kn
  "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
@prefix kn: <https://knotation.org/kn/>
@prefix ex: <http://example.com/>

: ex:p
rdfs:label: p

: ex:o
rdfs:label: o

: ex:s
p; kn:link: o
p: o
p; ex:d: o
p; @en: o")

(def test-A-ttl
  "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix kn: <https://knotation.org/kn/> .
@prefix ex: <http://example.com/> .

ex:p
  rdfs:label \"p\" .

ex:o
  rdfs:label \"o\" .

ex:s
  ex:p ex:o ;
  ex:p \"o\" ;
  ex:p \"o\"^^ex:d ;
  ex:p \"o\"@en .
")

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

; TODO: This should not be required.
(defn normalize-trailing-newlines [s]
  (str (clojure.string/trim s) "\n"))

(defn test-kn-roundtrip
  [s]
  (->> s
       (kn/read-input {})
       (kn/render-states {})
       st/render-output-string
       normalize-trailing-newlines
       (= (normalize-trailing-newlines s))
       is))

(deftest test-kn
  (test-kn-roundtrip test-0-kn)
  (test-kn-roundtrip test-A-kn))

(defn test-ttl-roundtrip
  [s]
  (->> s
       (api/read-string :ttl {})
       st/sequential-blank-nodes
       (ttl/render-states {})
       st/render-output-string
       (= s)
       is))

(deftest test-ttl
  (test-ttl-roundtrip test-0-ttl)
  (test-ttl-roundtrip test-1-ttl)
  (test-ttl-roundtrip test-2-ttl)
  (test-ttl-roundtrip test-3-ttl))

(defn test-kn->ttl
  [k t]
  (->> k
       (kn/read-input {})
       st/sequential-blank-nodes
       (ttl/render-states {})
       (map ::st/output)
       (map ::st/content)
       string/join
       (= t)
       is))

(defn test-ttl->kn
  [t k]
  (->> t
       (api/read-string :ttl {})
       (kn/render-states {})
       st/render-output-string
       normalize-trailing-newlines
       (= (normalize-trailing-newlines k))
       is))

(defn test-kn-ttl-roundtrip
  [k t]
  (test-kn->ttl k t)
  (test-ttl->kn t k))

(deftest test-kn-ttl
  (test-kn->ttl test-0-kn test-0-ttl)
  (test-kn->ttl test-A-kn test-A-ttl))

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
       st/render-output-string
       println)
