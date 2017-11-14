(ns org.knotation.tsv-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [org.knotation.rdf :as rdf]
            [org.knotation.state :as st]
            [org.knotation.tsv :as tsv]))

(stest/instrument)

(def example-state-1
  (-> st/default-state
      (st/add-prefix "ex" "https://example.com/")
      (st/add-label "homepage" "https://example.com/homepage")
      (st/add-datatype "https://example.com/homepage" "https://knotation.org/datatype/link")))

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

(defn test-line
  [line before-state after-fn]
  (let [input {::st/format :tsv
               ::st/line-number 1
               ::st/lines [line]}]
    (is (= (assoc (after-fn before-state) ::st/input input)
           (tsv/block->state (assoc before-state ::st/input input)))
        (str "Testing line: " line))))

(deftest test-tsv
  (test-line
   "@subject	label	homepage"
   example-state-1
   #(assoc % ::tsv/columns example-columns-1))

  (test-line
   "ex:foo	Foo	https://example.com"
   (assoc example-state-1 ::tsv/columns example-columns-1)
   #(-> %
        (st/add-label "Foo" "https://example.com/foo")
        (assoc
         ::rdf/subject {::rdf/iri "https://example.com/foo"}
         ::rdf/quads
         [{::rdf/graph nil
           ::rdf/subject {::rdf/iri "https://example.com/foo"}
           ::rdf/predicate {::rdf/iri "http://www.w3.org/2000/01/rdf-schema#label"}
           ::rdf/object {::rdf/lexical "Foo"}}
          {::rdf/graph nil
           ::rdf/subject {::rdf/iri "https://example.com/foo"}
           ::rdf/predicate {::rdf/iri "https://example.com/homepage"}
           ::rdf/object {::rdf/iri "https://example.com"}}]))))
