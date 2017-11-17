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

(def example-quad-ex-text-en-foo
  {::rdf/graph nil
   ::rdf/subject {::rdf/iri "https://example.com/foo"}
   ::rdf/predicate {::rdf/iri "https://example.com/text"}
   ::rdf/object {::rdf/lexical "Foo" ::rdf/language "en"}})

(def example-quad-ex-text-bar-foo
  {::rdf/graph nil
   ::rdf/subject {::rdf/iri "https://example.com/foo"}
   ::rdf/predicate {::rdf/iri "https://example.com/text"}
   ::rdf/object {::rdf/lexical "Foo"
                 ::rdf/datatype "https://example.com/bar"}})

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
           (kn/read-state (assoc before-state ::st/input input)))
        (str "Testing line: " line))))

(deftest test-read-state
  (test-line
   "@prefix ex: <https://example.com/>"
   st/blank-state
   #(-> %
        (st/add-prefix "ex" "https://example.com/")
        (assoc ::st/event ::st/prefix ::st/prefix "ex")))

  (test-line
   ": ex:foo"
   (-> st/blank-state
       (st/add-prefix "ex" "https://example.com/"))
   #(assoc
     %
     ::st/event ::st/subject-start
     ::rdf/subject {::rdf/iri "https://example.com/foo"}))

  (test-line
   "ex:text: Foo"
   (-> st/blank-state
       (st/add-prefix "ex" "https://example.com/")
       (assoc ::rdf/subject {::rdf/iri "https://example.com/foo"}))
   #(assoc
     %
     ::st/event ::st/statement
     ::rdf/quads [example-quad-ex-text-foo]))

  (test-line
   "ex:text; @en: Foo"
   (-> st/blank-state
       (st/add-prefix "ex" "https://example.com/")
       (assoc ::rdf/subject {::rdf/iri "https://example.com/foo"}))
   #(assoc
     %
     ::st/event ::st/statement
     ::rdf/quads [example-quad-ex-text-en-foo]))

  (test-line
   "ex:text; ex:bar: Foo"
   (-> st/blank-state
       (st/add-prefix "ex" "https://example.com/")
       (assoc ::rdf/subject {::rdf/iri "https://example.com/foo"}))
   #(assoc
     %
     ::st/event ::st/statement
     ::rdf/quads [example-quad-ex-text-bar-foo]))

  (test-line
   "ex:text: Foo"
   (-> st/blank-state
       (st/add-prefix "ex" "https://example.com/")
       (st/set-language "https://example.com/text" "en")
       (assoc ::rdf/subject {::rdf/iri "https://example.com/foo"}))
   #(assoc
     %
     ::st/event ::st/statement
     ::rdf/quads [example-quad-ex-text-en-foo]))

  (test-line
   "ex:text; @en: Foo"
   (-> st/blank-state
       (st/add-prefix "ex" "https://example.com/")
       (st/set-language "https://example.com/text" "fr")
       (assoc ::rdf/subject {::rdf/iri "https://example.com/foo"}))
   #(assoc
     %
     ::st/event ::st/statement
     ::rdf/quads [example-quad-ex-text-en-foo]))

  (test-line
   "ex:text: Foo"
   (-> st/blank-state
       (st/add-prefix "ex" "https://example.com/")
       (st/set-datatype "https://example.com/text"
                        "https://example.com/bar")
       (assoc ::rdf/subject {::rdf/iri "https://example.com/foo"}))
   #(assoc
     %
     ::st/event ::st/statement
     ::rdf/quads [example-quad-ex-text-bar-foo]))

  (test-line
   "ex:text; ex:bar: Foo"
   (-> st/blank-state
       (st/add-prefix "ex" "https://example.com/")
       (st/set-datatype "https://example.com/text"
                        "https://example.com/bat")
       (assoc ::rdf/subject {::rdf/iri "https://example.com/foo"}))
   #(assoc
     %
     ::st/event ::st/statement
     ::rdf/quads [example-quad-ex-text-bar-foo]))

  (test-line
   "label: Foo"
   (-> st/default-state
       (st/add-prefix "ex" "https://example.com/")
       (assoc ::rdf/subject {::rdf/iri "https://example.com/foo"}))
   #(-> %
        (st/add-label "Foo" "https://example.com/foo")
        (assoc ::st/event ::st/statement
               ::rdf/quads [example-quad-rdfs-label-foo])))

  (test-line
   "homepage: https://example.com"
   (-> st/default-state
       (st/add-label "homepage" "https://example.com/homepage")
       (st/set-datatype "https://example.com/homepage" "https://knotation.org/datatype/link")
       (assoc ::rdf/subject {::rdf/iri "https://example.com/foo"}))
   #(assoc
     %
     ::st/event ::st/statement
     ::rdf/quads [example-quad-homepage]))

  (test-line
   "default datatype: https://knotation.org/datatype/link"
   (-> st/default-state
       (assoc ::rdf/subject {::rdf/iri "https://example.com/foo"}))
   #(-> %
        (st/set-datatype "https://example.com/foo" "https://knotation.org/datatype/link")
        (assoc ::st/event ::st/statement
               ::rdf/quads [example-quad-default-datatype]))))

(def base-1 st/blank-state)
(def base-2
  (-> st/blank-state
      (st/add-prefix "ex" (rdf/ex))))

(def lines
  ["@prefix ex: <http://example.com/>"
   ""
   ": ex:s"
   "ex:p; ex:d: o"])

(def states
  [(assoc base-1
          ::st/event ::st/graph-start)
   (assoc base-2
          ::st/event ::st/prefix
          ::st/input
          {::st/format :kn
           ::st/line-number 1
           ::st/lines ["@prefix ex: <http://example.com/>"]}
          ::st/prefix "ex")
   (assoc base-2
          ::st/event ::st/space
          ::st/input
          {::st/format :kn
           ::st/line-number 2
           ::st/lines [""]})
   (assoc base-2
          ::st/event ::st/subject-start
          ::st/input
          {::st/format :kn
           ::st/line-number 3
           ::st/lines [": ex:s"]}
          ::rdf/subject {::rdf/iri (rdf/ex "s")})
   (assoc base-2
          ::st/event ::st/statement
          ::st/input
          {::st/format :kn
           ::st/line-number 4
           ::st/lines ["ex:p; ex:d: o"]}
          ::rdf/subject {::rdf/iri (rdf/ex "s")}
          ::rdf/quads
          [{::rdf/graph nil
            ::rdf/subject {::rdf/iri (rdf/ex "s")}
            ::rdf/predicate {::rdf/iri (rdf/ex "p")}
            ::rdf/object {::rdf/lexical "o"
                          ::rdf/datatype (rdf/ex "d")}}])
   (assoc base-2
          ::st/event ::st/subject-end
          ::rdf/subject {::rdf/iri (rdf/ex "s")})
   (assoc base-2
          ::st/event ::st/graph-end)])

(deftest test-states
  (is (s/valid? ::st/states states)))

(deftest test-read-lines
  (is (= states
         (kn/read-lines st/blank-state lines))))

(deftest test-render-lines
  (is (= lines
         (->> states
              kn/render-states
              (map ::st/output)
              (mapcat ::st/lines)))))
