(ns org.knotation.tsv-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]
            [org.knotation.state-spec]
            [org.knotation.tsv :as tsv]))

(stest/instrument)

(def base-1
  (-> st/blank-state
      (st/add-prefix "ex" (rdf/ex))))

(def lines
  ["	ex:p	 a"
   "ex:s	o	b"])

(def states
  [(assoc base-1
          ::st/event ::st/graph-start)
   (assoc base-1
          ::st/event ::st/header
          ::st/input
          {::st/format :tsv
           ::st/line-number 1
           ::st/lines ["	ex:p	 a"]})
   (assoc base-1
          ::st/event ::st/subject-start
          ::rdf/subject {::rdf/iri (rdf/ex "s")}
          ::st/input
          {::st/format :tsv
           ::st/line-number 2
           ::st/column-number 1
           ::st/lines [": ex:s"]})
   (assoc base-1
          ::st/event ::st/statement
          ::st/input
          {::st/format :tsv
           ::st/line-number 2
           ::st/column-number 2
           ::st/lines ["ex:p: o" " a: b"]}
          ::rdf/subject {::rdf/iri (rdf/ex "s")}
          ::rdf/quads
          [{::rdf/graph nil
            ::rdf/subject {::rdf/iri (rdf/ex "s")}
            ::rdf/predicate {::rdf/iri (rdf/ex "p")}
            ::rdf/object {::rdf/lexical "o
a: b"}}])
   (assoc base-1
          ::st/event ::st/subject-end
          ::rdf/subject {::rdf/iri (rdf/ex "s")})
   (assoc base-1
          ::st/event ::st/graph-end)])

;(deftest test-states)
;  (is (s/valid? ::st/states states)))

(deftest test-read-input
  (is (= states
         (tsv/read-input (::en/env base-1) {::st/lines lines}))))
