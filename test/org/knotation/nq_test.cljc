(ns org.knotation.nq-test
  (:require [clojure.test :refer [deftest is testing]]
            [#?(:clj clojure.spec.alpha :cljs cljs.spec.alpha) :as s]
            [#?(:clj clojure.spec.test.alpha :cljs cljs.spec.test.alpha) :as stest]
            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]
            [org.knotation.state-spec]
            [org.knotation.format :as fm]
            [org.knotation.omn :as omn]
            [org.knotation.nq :as nq]))

(stest/instrument)

(deftest test-read-quad
  (is (= {::rdf/graph {::rdf/iri "g"}
          ::rdf/subject {::rdf/iri "s"}
          ::rdf/predicate {::rdf/iri "p"}
          ::rdf/object {::rdf/iri "o"}}
         (nq/read-quad "<s> <p> <o> <g> .")))
  (is (= {::rdf/graph nil
          ::rdf/subject {::rdf/iri "s"}
          ::rdf/predicate {::rdf/iri "p"}
          ::rdf/object {::rdf/iri "o"}}
         (nq/read-quad "<s> <p> <o> .")))
  (is (= {::rdf/graph nil
          ::rdf/subject {::rdf/bnode "_::rdf/b1"}
          ::rdf/predicate {::rdf/iri "p"}
          ::rdf/object {::rdf/bnode "_::rdf/b2"}}
         (nq/read-quad "_::rdf/b1 <p> _::rdf/b2 .")))
  (is (= {::rdf/graph nil
          ::rdf/subject {::rdf/iri "s"}
          ::rdf/predicate {::rdf/iri "p"}
          ::rdf/object {::rdf/lexical "o"}}
         (nq/read-quad "<s> <p> \"o\" .")))
  (is (= {::rdf/graph nil
          ::rdf/subject {::rdf/iri "s"}
          ::rdf/predicate {::rdf/iri "p"}
          ::rdf/object {::rdf/lexical "o" ::rdf/language "en"}}
         (nq/read-quad "<s> <p> \"o\"@en .")))
  (is (= {::rdf/graph nil
          ::rdf/subject {::rdf/iri "s"}
          ::rdf/predicate {::rdf/iri "p"}
          ::rdf/object {::rdf/lexical "o" ::rdf/datatype "d"}}
         (nq/read-quad "<s> <p> \"o\"^^<d> .")))
  (is (= {::rdf/graph nil
          ::rdf/subject {::rdf/iri "s"}
          ::rdf/predicate {::rdf/iri "p"}
          ::rdf/object {::rdf/lexical "o
p
q"}}
         (nq/read-quad "<s> <p> \"o\\np\\nq\" ."))))

(def lines
  ["<s> <p> <o> ."
   "<s> <p> \"o\" <g> ."])

(def states
  [(assoc st/blank-state
          ::st/event ::st/graph-start
          ::rdf/graph nil)
   (assoc st/blank-state
          ::st/event ::st/subject-start
          ::rdf/graph nil
          ::rdf/subject {::rdf/iri "s"})
   (assoc st/blank-state
          ::st/event ::st/statement
          ::st/input
          {::st/format :nq
           ::st/line-number 1
           ::st/lines ["<s> <p> <o> ."]}
          ::rdf/graph nil
          ::rdf/subject {::rdf/iri "s"}
          ::rdf/predicate {::rdf/iri "p"}
          ::rdf/object {::rdf/iri "o"})
   (assoc st/blank-state
          ::st/event ::st/subject-end
          ::rdf/graph nil
          ::rdf/subject {::rdf/iri "s"})
   (assoc st/blank-state
          ::st/event ::st/graph-end
          ::rdf/graph nil)
   (assoc st/blank-state
          ::st/event ::st/graph-start
          ::rdf/graph {::rdf/iri "g"})
   (assoc st/blank-state
          ::st/event ::st/subject-start
          ::rdf/graph {::rdf/iri "g"}
          ::rdf/subject {::rdf/iri "s"})
   (assoc st/blank-state
          ::st/event ::st/statement
          ::st/input
          {::st/format :nq
           ::st/line-number 2
           ::st/lines ["<s> <p> \"o\" <g> ."]}
          ::rdf/graph {::rdf/iri "g"}
          ::rdf/subject {::rdf/iri "s"}
          ::rdf/predicate {::rdf/iri "p"}
          ::rdf/object {::rdf/lexical "o"})
   (assoc st/blank-state
          ::st/event ::st/subject-end
          ::rdf/graph {::rdf/iri "g"}
          ::rdf/subject {::rdf/iri "s"})
   (assoc st/blank-state
          ::st/event ::st/graph-end
          ::rdf/graph {::rdf/iri "g"})])

(deftest test-states
  (is (s/valid? ::st/states states)))

(deftest test-read-input
  (is (= states
         (nq/read-input en/blank-env {::st/lines lines}))))

(def example-typed-quad
  {::st/event ::st/statement
   ::rdf/graph nil
   ::rdf/subject {::rdf/iri "s"}
   ::rdf/predicate {::rdf/iri "p"}
   ::rdf/object {::rdf/lexical "o" ::rdf/datatype "d"}})

(deftest test-render-state
  (is (= (assoc
          example-typed-quad
          ::st/output
          {::st/format :nq
           ::st/lines ["<s> <p> \"o\"^^<d> ."]})
         (nq/render-state example-typed-quad))))

(deftest test-render-states
  (is (= [(assoc
           example-typed-quad
           ::st/output
           {::st/format :nq
            ::st/line-number 1
            ::st/lines ["<s> <p> \"o\"^^<d> ."]})]
         (nq/render-states [example-typed-quad])))
  (is (= lines
         (->> states
              nq/render-states
              (map ::st/output)
              (mapcat ::st/lines)))))

(def env-1
  (-> en/blank-env
      (en/add-label "has part" "http://purl.obolibrary.org/obo/BFO_0000050")
      (en/add-label "car" (rdf/ex "car"))
      (en/add-label "engine" (rdf/ex "engine"))
      (en/add-label "wheel" (rdf/ex "wheel"))))

(def ex-manchester
  "'has part' some (engine and 'has part' some wheel)")

(def ex-subclass
  "<http://example.com/car> <http://www.w3.org/2000/01/rdf-schema#subClassOf> _:1 .
_:1 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#Restriction> .
_:1 <http://www.w3.org/2002/07/owl#onProperty> <http://purl.obolibrary.org/obo/BFO_0000050> .
_:1 <http://www.w3.org/2002/07/owl#someValuesFrom> _:2 .
_:2 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#Class> .
_:2 <http://www.w3.org/2002/07/owl#intersectionOf> _:3 .
_:3 <http://www.w3.org/1999/02/22-rdf-syntax-ns#first> <http://example.com/engine> .
_:3 <http://www.w3.org/1999/02/22-rdf-syntax-ns#rest> _:4 .
_:4 <http://www.w3.org/1999/02/22-rdf-syntax-ns#first> _:5 .
_:4 <http://www.w3.org/1999/02/22-rdf-syntax-ns#rest> <http://www.w3.org/1999/02/22-rdf-syntax-ns#nil> .
_:5 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#Restriction> .
_:5 <http://www.w3.org/2002/07/owl#onProperty> <http://purl.obolibrary.org/obo/BFO_0000050> .
_:5 <http://www.w3.org/2002/07/owl#someValuesFrom> <http://example.com/wheel> .")

(def ex-annotations
  "<http://example.com/car> <http://www.w3.org/2000/01/rdf-schema#label> \"car\" .
_:1 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#Axiom> .
_:1 <http://www.w3.org/2002/07/owl#annotatedSource> <http://example.com/car> .
_:1 <http://www.w3.org/2002/07/owl#annotatedProperty> <http://www.w3.org/2000/01/rdf-
sc1abel> .
_:1 <http://www.w3.org/2002/07/owl#annotatedTarget> \"car\" .
_:1 <http://www.w3.org/2000/01/rdf-schema#comment> _:2
_:2 <http://www.w3.org/1999/02/22-rdf-syntax-ns#first> \"A\" .
_:2 <http://www.w3.org/1999/02/22-rdf-syntax-ns#rest> _:3 .
_:3 <http://www.w3.org/1999/02/22-rdf-syntax-ns#first> \"B\" .
_:3 <http://www.w3.org/1999/02/22-rdf-syntax-ns#rest> <http://www.w3.org/1999/02/22-rdf-syntax-ns#nil> .
_:4 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#Axiom> .
_:4 <http://www.w3.org/2002/07/owl#annotatedSource> _:1 .
_:4 <http://www.w3.org/2002/07/owl#annotatedProperty> <http://www.w3.org/2000/01/rdf-comment> .
_:4 <http://www.w3.org/2002/07/owl#annotatedTarget> \"A\" .
_:4 <http://www.w3.org/2000/01/rdf-schema#comment> \"AA\" .")

(deftest test-render-branch
  (->> ex-manchester
       omn/parse-class-expression
       (omn/convert-class-expression env-1)
       (assoc
        {::rdf/subject {::rdf/iri (rdf/ex "car")}
         ::rdf/predicate {::rdf/iri (rdf/rdfs "subClassOf")}}
        ::rdf/object)
       rdf/unbranch-quad
       rdf/sequential-blank-nodes
       (map nq/render-quad)
       (= (clojure.string/split-lines ex-subclass))
       is)
  (->> ex-subclass
       clojure.string/split-lines
       shuffle ; order should not matter
       (map nq/read-quad)
       rdf/branch-quads
       first
       ::rdf/object
       (omn/render-class-expression env-1)
       omn/write-class-expression
       (= ex-manchester)
       is))
