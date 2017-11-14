(ns org.knotation.kn-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [org.knotation.rdf :as rdf]
            [org.knotation.state :as st]
            [org.knotation.kn :as kn]))

(stest/instrument)

(def example-quad-ex-text-foo
  {::rdf/graph nil
   ::rdf/subject {::rdf/iri "https://example.com/foo"}
   ::rdf/predicate {::rdf/iri "https://example.com/text"}
   ::rdf/object {::rdf/lexical "Foo"}})

(def example-quad-rdfs-label-foo
  {::rdf/graph nil
   ::rdf/subject {::rdf/iri "https://example.com/foo"}
   ::rdf/predicate {::rdf/iri "http://www.w3.org/2000/01/rdf-schema#label"}
   ::rdf/object {::rdf/lexical "Foo"}})

(def example-quad-homepage
  {::rdf/graph nil
   ::rdf/subject {::rdf/iri "https://example.com/foo"}
   ::rdf/predicate {::rdf/iri "https://example.com/homepage"}
   ::rdf/object {::rdf/iri "https://example.com"}})

(def example-quad-default-datatype
  {::rdf/graph nil
   ::rdf/subject {::rdf/iri "https://example.com/foo"}
   ::rdf/predicate {::rdf/iri "https://knotation.org/predicate/default-datatype"}
   ::rdf/object {::rdf/iri "https://knotation.org/datatype/link"}})

(defn test-line
  [line before-state after-fn]
  (let [input {::st/format :knotation
               ::st/line-number 1
               ::st/lines [line]}]
    (is (= (assoc (after-fn before-state) ::st/input input)
           (kn/block->state (assoc before-state ::st/input input)))
        (str "Testing line: " line))))

(deftest test-block->state
  (test-line
   "@prefix ex: <https://example.com/>"
   st/blank-state
   #(st/add-prefix % "ex" "https://example.com/"))

  (test-line
   ": ex:foo"
   (-> st/blank-state
       (st/add-prefix "ex" "https://example.com/"))
   #(assoc % ::rdf/subject {::rdf/iri "https://example.com/foo"}))

  (test-line
   "ex:text: Foo"
   (-> st/blank-state
       (st/add-prefix "ex" "https://example.com/")
       (assoc ::rdf/subject {::rdf/iri "https://example.com/foo"}))
   #(assoc % ::rdf/quads [example-quad-ex-text-foo]))

  (test-line
   "label: Foo"
   (-> st/default-state
       (st/add-prefix "ex" "https://example.com/")
       (assoc ::rdf/subject {::rdf/iri "https://example.com/foo"}))
   #(-> %
        (st/add-label "Foo" "https://example.com/foo")
        (assoc ::rdf/quads [example-quad-rdfs-label-foo])))

  (test-line
   "homepage: https://example.com"
   (-> st/default-state
       (st/add-label "homepage" "https://example.com/homepage")
       (st/add-datatype "https://example.com/homepage" "https://knotation.org/datatype/link")
       (assoc ::rdf/subject {::rdf/iri "https://example.com/foo"}))
   #(assoc % ::rdf/quads [example-quad-homepage]))

  (test-line
   "default datatype: https://knotation.org/datatype/link"
   (-> st/default-state
       (assoc ::rdf/subject {::rdf/iri "https://example.com/foo"}))
   #(-> %
        (st/add-datatype "https://example.com/foo" "https://knotation.org/datatype/link")
        (assoc ::rdf/quads [example-quad-default-datatype]))))
