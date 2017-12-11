(ns org.knotation.ttl-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]
            [org.knotation.format :as fm]
            [org.knotation.omn :as omn]
            [org.knotation.ttl :as ttl]))

(stest/instrument)

(def example-typed-quad
  {::rdf/graph nil
   ::rdf/subject {::rdf/iri (rdf/ex "s")}
   ::rdf/predicate {::rdf/iri (rdf/ex "p")}
   ::rdf/object {::rdf/lexical "o" ::rdf/datatype (rdf/ex "d")}})

(def example-multiline-quad
  {::rdf/graph nil
   ::rdf/subject {::rdf/iri (rdf/ex "s")}
   ::rdf/predicate {::rdf/iri (rdf/ex "p")}
   ::rdf/object {::rdf/lexical "multi
line"}})

(def env-1
  (-> en/blank-env
      (en/add-prefix "ex" (rdf/ex))))

(deftest test-render-state
  (is (= {::st/event ::st/statement
          ::en/env env-1
          ::rdf/quads [example-typed-quad]
          ::st/output
          {::st/format :ttl
           ::st/lines ["  ex:p \"o\"^^ex:d ;"]}}
         (ttl/render-state
          {::st/event ::st/statement
           ::en/env env-1
           ::rdf/quads [example-typed-quad]})))
  (is (= {::st/event ::st/statement
          ::en/env env-1
          ::rdf/quads [example-multiline-quad]
          ::st/output
          {::st/format :ttl
           ::st/lines ["  ex:p \"\"\"multi"
                       "line\"\"\" ;"]}}
         (ttl/render-state
          {::st/event ::st/statement
           ::en/env env-1
           ::rdf/quads [example-multiline-quad]}))))

;(deftest test-indent
;  (is (= [{::st/output {::st/lines ["  A ;" "  B ;"]}}
;          {::st/output {::st/lines ["  C ."]}}
;          {}]
;         (ttl/indent-states
;          [{::st/output {::st/lines ["A" "B"]}}
;           {::st/output {::st/lines ["C"]}}
;           {}]))))

(def input-states
  [{::st/event ::st/prefix
    ::en/env env-1
    ::st/prefix "ex"}
   {::st/event ::st/space
    ::en/env env-1}
   {::st/event ::st/subject-start
    ::en/env env-1
    ::rdf/subject {::rdf/iri (rdf/ex "s")}}
   {::st/event ::st/statement
    ::en/env env-1
    ::rdf/subject {::rdf/iri (rdf/ex "s")}
    ::rdf/quads [example-typed-quad]}
   {::st/event ::st/statement
    ::en/env env-1
    ::rdf/subject {::rdf/iri (rdf/ex "s")}
    ::rdf/quads [example-typed-quad]}
   {::st/event ::st/subject-end
    ::en/env env-1
    ::rdf/subject {::rdf/iri (rdf/ex "s")}}])

(def output-states
  [{::st/event ::st/prefix
    ::en/env env-1
    ::st/prefix "ex"
    ::st/output
    {::st/format :ttl
     ::st/line-number 1
     ::st/lines ["@prefix ex: <http://example.com/> ."]}}
   {::st/event ::st/space
    ::en/env env-1
    ::st/output
    {::st/format :ttl
     ::st/line-number 2
     ::st/lines [""]}}
   {::st/event ::st/subject-start
    ::en/env env-1
    ::rdf/subject {::rdf/iri (rdf/ex "s")}
    ::st/output
    {::st/format :ttl
     ::st/line-number 3
     ::st/lines ["ex:s"]}}
   {::st/event ::st/statement
    ::en/env env-1
    ::rdf/subject {::rdf/iri (rdf/ex "s")}
    ::rdf/quads [example-typed-quad]
    ::st/output
    {::st/format :ttl
     ::st/line-number 4
     ::st/lines ["  ex:p \"o\"^^ex:d ;"]}}
   {::st/event ::st/statement
    ::en/env env-1
    ::rdf/subject {::rdf/iri (rdf/ex "s")}
    ::rdf/quads [example-typed-quad]
    ::st/output
    {::st/format :ttl
     ::st/line-number 5
     ::st/lines ["  ex:p \"o\"^^ex:d ."]}}
   {::st/event ::st/subject-end
    ::en/env env-1
    ::rdf/subject {::rdf/iri (rdf/ex "s")}}])

(def lines
  ["@prefix ex: <http://example.com/> ."
   ""
   "ex:s"
   "  ex:p \"o\"^^ex:d ;"
   "  ex:p \"o\"^^ex:d ."])

(deftest test-states
  (is (s/valid? ::st/states input-states))
  (is (s/valid? ::st/states output-states)))

(deftest test-render-states
  (is (= output-states
         (ttl/render-states input-states)))
  (is (= lines
         (->> output-states
              (map ::st/output)
              (mapcat ::st/lines))))
  (is (= lines
         (->> input-states
              ttl/render-states
              (map ::st/output)
              (mapcat ::st/lines)))))
