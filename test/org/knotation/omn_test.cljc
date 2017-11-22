(ns org.knotation.omn-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.omn :as omn]))

(stest/instrument)

(def ex-list-branch
  [{::rdf/subject {::rdf/bnode "_:1"}
    ::rdf/predicate {::rdf/iri (rdf/rdf "first")}
    ::rdf/object {::rdf/literal "1"}}
   {::rdf/subject {::rdf/bnode "_:1"}
    ::rdf/predicate {::rdf/iri (rdf/rdf "rest")}
    ::rdf/object
    {::rdf/bnode "_:2"
     ::rdf/pairs
     [{::rdf/predicate {::rdf/iri (rdf/rdf "first")}
       ::rdf/object {::rdf/literal "2"}}
      {::rdf/predicate {::rdf/iri (rdf/rdf "rest")}
       ::rdf/object
       {::rdf/bnode "_:3"
        ::rdf/pairs
        [{::rdf/predicate {::rdf/iri (rdf/rdf "first")}
          ::rdf/object {::rdf/literal "3"}}
         {::rdf/predicate {::rdf/iri (rdf/rdf "rest")}
          ::rdf/object {::rdf/iri (rdf/rdf "nil")}}]}}]}}])

(def ex-list
  (->> [1 2 3]
       (map str)
       (map (fn [x] {::rdf/literal x}))))

(def ex-long-list
  (->> (range 1 10000)
       (map str)
       (map (fn [x] {::rdf/literal x}))))

(deftest test-branch->list
  (is (=  ex-list
          (->> ex-list-branch
               omn/branch->list))))

(deftest test-list->branch
  (is (= ex-list
         (->> ex-list
              omn/list->branch
              ::rdf/pairs
              omn/branch->list)))
  (is (= ex-list
         (->> ex-list-branch
              (mapcat rdf/unbranch-quad)
              (map ::rdf/object)
              (filter ::rdf/literal))))
  (is (= ex-long-list
         (->> ex-long-list
              omn/list->branch
              ::rdf/pairs
              omn/branch->list)))
  (is (= ex-long-list
         (->> ex-long-list
              omn/list->branch
              (assoc {} ::rdf/object)
              rdf/unbranch-quad
              (map ::rdf/object)
              (filter ::rdf/literal)))))

(def env-1
  (-> en/blank-env
      (en/add-label "foo" (rdf/ex "foo"))
      (en/add-label "bar" (rdf/ex "bar"))
      (en/add-label "foo bar" (rdf/ex "foo-bar"))
      (en/add-label "has part" (rdf/ex "has-part"))
      (en/add-label "is about" (rdf/ex "is-about"))
      (en/add-label "has_specified_output" (rdf/ex "has_specified_output"))
      (en/add-label "material entity" (rdf/ex "material-entity"))
      (en/add-label "information content entity" (rdf/ex "information-content-entity"))
      (en/add-label "has role" (rdf/ex "has-role"))
      (en/add-label "evaluant role" (rdf/ex "evaluant-role"))))

(defn test-round-trip
  [content]
  (->> content
       omn/parse-class-expression
       (omn/convert-class-expression env-1)
       (omn/render-class-expression env-1)
       omn/write-class-expression
       (= content)
       is))

(deftest test-round-trips
  (test-round-trip "foo")
  (test-round-trip "'foo bar'")
  (test-round-trip "not foo")
  (test-round-trip "foo or bar")
  (test-round-trip "foo or foo or foo")
  (test-round-trip "foo and bar")
  (test-round-trip "'has part' some foo")
  (test-round-trip "'has part' only foo")
  (test-round-trip "'has part' some (foo or bar)")
  ; TODO: handle brackets and indentation better
  (test-round-trip "'is about' some ('material entity' and 'has role' some 'evaluant role')")
  (test-round-trip "has_specified_output some ('information content entity' and 'is about' some ('material entity' and 'has role' some 'evaluant role'))"))
