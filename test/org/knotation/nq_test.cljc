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

(def lines
  ["<s> <p> <o> ."
   "<s> <p> \"o\" <g> ."])

(def states
  [(assoc st/blank-state
          ::st/event ::st/graph-start)
   (assoc st/blank-state
          ::st/event ::st/subject-start
          ::rdf/subject {::rdf/iri "s"})
   (assoc st/blank-state
          ::st/event ::st/statement
          ::st/input
          {::st/format :nq
           ::st/line-number 1
           ::st/lines ["<s> <p> <o> ."]}
          ::rdf/subject {::rdf/iri "s"}
          ::rdf/quads
          [{::rdf/graph nil
            ::rdf/subject {::rdf/iri "s"}
            ::rdf/predicate {::rdf/iri "p"}
            ::rdf/object {::rdf/iri "o"}}])
   (assoc st/blank-state
          ::st/event ::st/subject-end
          ::rdf/subject {::rdf/iri "s"})
   (assoc st/blank-state
          ::st/event ::st/graph-end)
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
          ::rdf/quads
          [{::rdf/graph {::rdf/iri "g"}
            ::rdf/subject {::rdf/iri "s"}
            ::rdf/predicate {::rdf/iri "p"}
            ::rdf/object {::rdf/lexical "o"}}])
   (assoc st/blank-state
          ::st/event ::st/subject-end
          ::rdf/graph {::rdf/iri "g"}
          ::rdf/subject {::rdf/iri "s"})
   (assoc st/blank-state
          ::st/event ::st/graph-end
          ::rdf/graph {::rdf/iri "g"})])

(deftest test-states
  (is (s/valid? ::st/states states)))

(deftest test-read-lines
  (is (= states
         (nq/read-lines st/blank-state lines))))

(def example-typed-quad
  {::rdf/graph nil
   ::rdf/subject {::rdf/iri "s"}
   ::rdf/predicate {::rdf/iri "p"}
   ::rdf/object {::rdf/lexical "o" ::rdf/datatype "d"}})

(deftest test-render-state
  (is (= {::st/event ::st/statement
          ::rdf/quads [example-typed-quad]
          ::st/output
          {::st/format :nq
           ::st/lines ["<s> <p> \"o\"^^<d> ."]}}
         (nq/render-state
          {::st/event ::st/statement
           ::rdf/quads [example-typed-quad]}))))

(deftest test-render-states
  (is (= [{::st/event ::st/statement
           ::rdf/quads [example-typed-quad]
           ::st/output
           {::st/format :nq
            ::st/line-number 1
            ::st/lines ["<s> <p> \"o\"^^<d> ."]}}]
         (nq/render-states
          [{::st/event ::st/statement
            ::rdf/quads [example-typed-quad]}])))
  (is (= lines
         (->> states
              nq/render-states
              (map ::st/output)
              (mapcat ::st/lines)))))
