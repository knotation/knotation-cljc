(ns org.knotation.nq-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [org.knotation.rdf :as rdf]
            [org.knotation.state :as st]
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
         (nq/read-quad "<s> <p> \"o\"^^<d> ."))))

(def example-typed-quad
  {::rdf/graph nil
   ::rdf/subject {::rdf/iri "s"}
   ::rdf/predicate {::rdf/iri "p"}
   ::rdf/object {::rdf/lexical "o" ::rdf/datatype "d"}})

(deftest test-render-state
  (is (= {::rdf/quads [example-typed-quad]
          ::st/output-line-count 1
          ::st/output
          {::st/format :nq
           ::st/line-number 1
           ::st/lines ["<s> <p> \"o\"^^<d> ."]}}
         (nq/render-state
          {::rdf/quads [example-typed-quad]}))))

(deftest test-render-states
  (is (= [{::rdf/quads [example-typed-quad]
           ::st/output-line-count 1
           ::st/output
           {::st/format :nq
            ::st/line-number 1
            ::st/lines ["<s> <p> \"o\"^^<d> ."]}}]
         (nq/render-states
          [{::rdf/quads [example-typed-quad]}]))))
