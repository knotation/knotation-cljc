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

(def example-state-1
  (-> st/default-state
      (st/add-prefix "ex" "https://example.com/")
      (st/add-label "homepage" "https://example.com/homepage")
      (st/set-datatype "https://example.com/homepage" "https://knotation.org/datatype/link")))

(def example-columns-1
  [{::tsv/column-number 1
    ::tsv/label "@subject"
    ::tsv/subject? true}
   {::tsv/column-number 2
    ::tsv/label "label"
    ::rdf/predicate {::rdf/iri "http://www.w3.org/2000/01/rdf-schema#label"}}
   {::tsv/column-number 3
    ::tsv/label "homepage"
    ::rdf/predicate {::rdf/iri "https://example.com/homepage"}}])

;(defn test-line
;  [line before-state after-fn]
;  (let [input {::st/format :tsv
;               ::st/line-number 1
;               ::st/lines [line]}]
;    (is (= (assoc (after-fn before-state) ::st/input input)
;           (tsv/read-state (assoc before-state ::st/input input)))
;        (str "Testing line: " line))))
;
;(deftest test-tsv
;  (test-line
;   "@subject	label	homepage"
;   example-state-1
;   #(assoc % ::tsv/columns example-columns-1))
;
;  (test-line
;   "ex:foo	Foo	https://example.com"
;   (assoc example-state-1 ::tsv/columns example-columns-1)
;   #(-> %
;        (st/add-label "Foo" "https://example.com/foo")
;        (assoc
;         ::rdf/subject {::rdf/iri "https://example.com/foo"}
;         ::rdf/quads
;         [{::rdf/graph nil
;           ::rdf/subject {::rdf/iri "https://example.com/foo"}
;           ::rdf/predicate {::rdf/iri "http://www.w3.org/2000/01/rdf-schema#label"}
;           ::rdf/object {::rdf/lexical "Foo"}}
;          {::rdf/graph nil
;           ::rdf/subject {::rdf/iri "https://example.com/foo"}
;           ::rdf/predicate {::rdf/iri "https://example.com/homepage"}
;           ::rdf/object {::rdf/iri "https://example.com"}}]))))

(def base-1
  (-> st/blank-state
      (st/add-prefix "ex" (rdf/ex))))

(def columns-1
  [{::tsv/column-number 1
    ::tsv/label "@subject"
    ::tsv/subject? true}
   {::tsv/column-number 2
    ::tsv/label "ex:p"
    ::rdf/predicate {::rdf/iri (rdf/ex "p")}}])

(def lines
  ["@subject	ex:p"
   "ex:s	o"])

(def states
  [(assoc base-1
          ::st/event ::st/graph-start)
   (assoc base-1
          ::st/event ::st/header
          ::tsv/columns columns-1
          ::st/input
          {::st/format :tsv
           ::st/line-number 1
           ::st/lines ["@subject	ex:p"]})
   (assoc base-1
          ::st/event ::st/subject-start
          ::tsv/columns columns-1
          ::rdf/subject {::rdf/iri (rdf/ex "s")}
          ::st/input
          {::st/format :tsv
           ::st/line-number 2
           ::st/lines ["ex:s	o"]})
   (assoc base-1
          ::st/event ::st/statement
          ::tsv/columns columns-1
          ::st/input
          {::st/format :tsv
           ::st/line-number 2
           ::st/column-number 2
           ::st/lines ["ex:s	o"]}
          ::rdf/subject {::rdf/iri (rdf/ex "s")}
          ::rdf/quads
          [{::rdf/graph nil
            ::rdf/subject {::rdf/iri (rdf/ex "s")}
            ::rdf/predicate {::rdf/iri (rdf/ex "p")}
            ::rdf/object {::rdf/lexical "o"}}])
   (assoc base-1
          ::st/event ::st/subject-end
          ::tsv/columns columns-1
          ::rdf/subject {::rdf/iri (rdf/ex "s")})
   (assoc base-1
          ::st/event ::st/graph-end)])

;(deftest test-states)
;  (is (s/valid? ::st/states states)))

(deftest test-read-input
  (is (= states
         (tsv/read-input (::en/env base-1) {::st/lines lines}))))
