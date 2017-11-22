(ns org.knotation.rdf-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [org.knotation.rdf :as rdf]))

(stest/instrument)

(def ex-flat
  [{::rdf/subject {::rdf/iri "s"}
    ::rdf/predicate {::rdf/iri "p"}
    ::rdf/object {::rdf/bnode "_:1"}}
   {::rdf/subject {::rdf/bnode "_:1"}
    ::rdf/predicate {::rdf/iri "p"}
    ::rdf/object {::rdf/bnode "_:2"}}
   {::rdf/subject {::rdf/bnode "_:1"}
    ::rdf/predicate {::rdf/iri "p"}
    ::rdf/object {::rdf/literal "1"}}
   {::rdf/subject {::rdf/bnode "_:2"}
    ::rdf/predicate {::rdf/iri "p"}
    ::rdf/object {::rdf/literal "2"}}])

(def ex-branched
  {::rdf/subject {::rdf/iri "s"}
   ::rdf/predicate {::rdf/iri "p"}
   ::rdf/object
   {::rdf/bnode "_:A"
    ::rdf/pairs
    [{::rdf/predicate {::rdf/iri "p"}
      ::rdf/object
      {::rdf/bnode "_:B"
       ::rdf/pairs
       [{::rdf/predicate {::rdf/iri "p"}
         ::rdf/object {::rdf/literal "2"}}]}}
     {::rdf/predicate {::rdf/iri "p"}
      ::rdf/object {::rdf/literal "1"}}]}})

(deftest test-unbranch
  (is (= ex-flat
         (->> ex-branched
              rdf/unbranch-quad
              rdf/sequential-blank-nodes)))
  (is (= ex-flat
         (->> ex-branched
              vector
              rdf/sequential-blank-nodes
              first
              rdf/unbranch-quad))))
