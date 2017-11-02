(ns org.knotation.kn-test
  (:require [clojure.test :refer [deftest is testing]]
            [org.knotation.state :as st]
            [org.knotation.kn :as kn]))

(def example-quad-ex-text-foo
  {:graph nil
   :subject {:iri "https://example.com/foo"}
   :predicate {:iri "https://example.com/text"}
   :object {:lexical "Foo"}})

(def example-quad-rdfs-label-foo
  {:graph nil
   :subject {:iri "https://example.com/foo"}
   :predicate {:iri "http://www.w3.org/2000/01/rdf-schema#label"}
   :object {:lexical "Foo"}})

(def example-quad-homepage
  {:graph nil
   :subject {:iri "https://example.com/foo"}
   :predicate {:iri "https://example.com/homepage"}
   :object {:iri "https://example.com"}})

(def example-quad-default-datatype
  {:graph nil
   :subject {:iri "https://example.com/foo"}
   :predicate {:iri "https://knotation.org/predicate/default-datatype"}
   :object {:iri "https://knotation.org/datatype/link"}})

(defn test-line
  [line before after-fn]
  (is (= (kn/block->state (assoc before :block [line]))
         (assoc (after-fn before) :block [line]))
      (str "Testing line: " line)))

(deftest test-block->state
  (test-line
   "@prefix ex: <https://example.com/>"
   st/blank-state
   #(st/add-prefix % "ex" "https://example.com/"))

  (test-line
   ": ex:foo"
   (-> st/blank-state
       (st/add-prefix "ex" "https://example.com/"))
   #(assoc % :subject {:iri "https://example.com/foo"}))

  (test-line
   "ex:text: Foo"
   (-> st/blank-state
       (st/add-prefix "ex" "https://example.com/")
       (assoc :subject {:iri "https://example.com/foo"}))
   #(assoc % :quads [example-quad-ex-text-foo]))

  (test-line
   "label: Foo"
   (-> st/default-state
       (st/add-prefix "ex" "https://example.com/")
       (assoc :subject {:iri "https://example.com/foo"}))
   #(-> %
        (st/add-label "Foo" "https://example.com/foo")
        (assoc :quads [example-quad-rdfs-label-foo])))

  (test-line
   "homepage: https://example.com"
   (-> st/default-state
       (st/add-label "homepage" "https://example.com/homepage")
       (st/add-datatype "https://example.com/homepage" "https://knotation.org/datatype/link")
       (assoc :subject {:iri "https://example.com/foo"}))
   #(assoc % :quads [example-quad-homepage]))

  (test-line
   "default datatype: https://knotation.org/datatype/link"
   (-> st/default-state
       (assoc :subject {:iri "https://example.com/foo"}))
   #(-> %
        (st/add-datatype "https://example.com/foo" "https://knotation.org/datatype/link")
        (assoc :quads [example-quad-default-datatype]))))
