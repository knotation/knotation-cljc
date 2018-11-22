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
  "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
@prefix owl: <http://www.w3.org/2002/07/owl#>
@prefix ex: <http://example.com/>

: ex:s
ex:a: A
> ex:b: B
>> ex:c: C
> ex:d: D")

(def test-2-ttl
  "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix ex: <http://example.com/> .

ex:s
  ex:a \"A\" .

_:b0
  rdf:type owl:Annotation ;
  owl:annotatedSource ex:s ;
  owl:annotatedProperty ex:a ;
  owl:annotatedTarget \"A\" ;
  ex:b \"B\" .

_:b1
  rdf:type owl:Annotation ;
  owl:annotatedSource _:b0 ;
  owl:annotatedProperty ex:b ;
  owl:annotatedTarget \"B\" ;
  ex:c \"C\" .

_:b2
  rdf:type owl:Annotation ;
  owl:annotatedSource ex:s ;
  owl:annotatedProperty ex:a ;
  owl:annotatedTarget \"A\" ;
  ex:d \"D\" .
")

(def test-3-kn
  "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
@prefix owl: <http://www.w3.org/2002/07/owl#>
@prefix kn: <https://knotation.org/kn/>
@prefix ex: <http://example.com/>

: ex:s
ex:before: before
ex:a; kn:anon:
 ex:b: B
 ex:c; kn:anon:
  ex:d: D
 ex:e: E
ex:after: after
")

(def test-3-ttl
  "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix kn: <https://knotation.org/kn/> .
@prefix ex: <http://example.com/> .

ex:s
  ex:before \"before\" ;
  ex:a [
    ex:b \"B\" ;
    ex:c [
      ex:d \"D\"
    ] ;
    ex:e \"E\"
  ] ;
  ex:after \"after\" .
")

(def test-4-ttl
  "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix ex: <http://example.com/> .

ex:s
  ex:p \"before\" ;
  ex:p _:b0 ;
  ex:p [
    ex:p \"inner\" ;
    ex:p [
      ex:p \"innermost\"
    ] ;
    ex:p \"innerafter\"
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
       (api/read-string :kn nil)
       (kn/render-states st/default-state)
       st/render-output-string
       normalize-trailing-newlines
       (= (normalize-trailing-newlines s))
       is))

(deftest test-kn
  (test-kn-roundtrip test-0-kn)
  (test-kn-roundtrip test-A-kn))
  ;(test-kn-roundtrip test-1-kn))

(defn test-ttl-roundtrip
  [s]
  (->> s
       (api/read-string :ttl nil)
       st/sequential-blank-nodes
       (ttl/render-states en/default-env)
       st/render-output-string
       (= s)
       is))

(deftest test-ttl
  (test-ttl-roundtrip test-0-ttl)
  (test-ttl-roundtrip test-1-ttl)
  (test-ttl-roundtrip test-2-ttl)
  (test-ttl-roundtrip test-3-ttl)
  (test-ttl-roundtrip test-4-ttl))

(defn test-kn->ttl
  [k t]
  (->> k
       (api/read-string :kn nil)
       st/sequential-blank-nodes
       (ttl/render-states en/default-env)
       (map ::st/output)
       (map ::st/content)
       string/join
       (= t)
       is))

(defn test-ttl->kn
  [t k]
  (->> t
       (api/read-string :ttl nil)
       (kn/render-states st/default-state)
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
  (test-kn-ttl-roundtrip test-A-kn test-A-ttl)
  (test-kn-ttl-roundtrip test-2-kn test-2-ttl)
  (test-kn-ttl-roundtrip test-3-kn test-3-ttl))
