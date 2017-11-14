(ns org.knotation.nq-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [org.knotation.rdf :as rdf]
            [org.knotation.state :as st]
            [org.knotation.nq :as nq]))

(stest/instrument)

(deftest test-parse-quad
  (is (= {::rdf/graph {::rdf/iri "g"}
          ::rdf/subject {::rdf/iri "s"}
          ::rdf/predicate {::rdf/iri "p"}
          ::rdf/object {::rdf/iri "o"}}
         (nq/parse-quad "<s> <p> <o> <g> .")))
  (is (= {::rdf/graph nil
          ::rdf/subject {::rdf/iri "s"}
          ::rdf/predicate {::rdf/iri "p"}
          ::rdf/object {::rdf/iri "o"}}
         (nq/parse-quad "<s> <p> <o> .")))
  (is (= {::rdf/graph nil
          ::rdf/subject {::rdf/bnode "_::rdf/b1"}
          ::rdf/predicate {::rdf/iri "p"}
          ::rdf/object {::rdf/bnode "_::rdf/b2"}}
         (nq/parse-quad "_::rdf/b1 <p> _::rdf/b2 .")))
  (is (= {::rdf/graph nil
          ::rdf/subject {::rdf/iri "s"}
          ::rdf/predicate {::rdf/iri "p"}
          ::rdf/object {::rdf/lexical "o"}}
         (nq/parse-quad "<s> <p> \"o\" .")))
  (is (= {::rdf/graph nil
          ::rdf/subject {::rdf/iri "s"}
          ::rdf/predicate {::rdf/iri "p"}
          ::rdf/object {::rdf/lexical "o" ::rdf/language "en"}}
         (nq/parse-quad "<s> <p> \"o\"@en .")))
  (is (= {::rdf/graph nil
          ::rdf/subject {::rdf/iri "s"}
          ::rdf/predicate {::rdf/iri "p"}
          ::rdf/object {::rdf/lexical "o" ::rdf/datatype "d"}}
         (nq/parse-quad "<s> <p> \"o\"^^<d> ."))))

(def example-typed-quad
  {::rdf/graph nil
   ::rdf/subject {::rdf/iri "s"}
   ::rdf/predicate {::rdf/iri "p"}
   ::rdf/object {::rdf/lexical "o" ::rdf/datatype "d"}})

(deftest test-process-output
  (is (= {::rdf/quads [example-typed-quad]
          ::st/output-line-count 1
          ::st/output
          {::st/format :nq
           ::st/line-number 1
           ::st/lines ["<s> <p> \"o\"^^<d> ."]}}
         (nq/process-output
          {::rdf/quads [example-typed-quad]}))))

(deftest test-process-outputs
  (is (= [{::rdf/quads [example-typed-quad]
           ::st/output-line-count 1
           ::st/output
           {::st/format :nq
            ::st/line-number 1
            ::st/lines ["<s> <p> \"o\"^^<d> ."]}}]
         (nq/process-outputs
          [{::rdf/quads [example-typed-quad]}]))))
