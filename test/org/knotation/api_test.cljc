(ns org.knotation.api-test
  (:require [clojure.test :refer [deftest is testing]]
            [#?(:clj clojure.spec.alpha :cljs cljs.spec.alpha) :as s]
            [#?(:clj clojure.spec.test.alpha :cljs cljs.spec.test.alpha) :as stest]
            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]
            [org.knotation.state-spec]
            [org.knotation.api-spec]
            [org.knotation.api :as api]))

(stest/instrument)

(def step-1
  "@prefix ex: <https://example.com/>

: ex:s
ex:p: o
foo: bar")

(def step-2
  {::api/operation-type :read
   ::st/format :kn
   ::st/line-number 1
   ::st/lines (clojure.string/split-lines step-1)})

(deftest test-kn
  (is (= step-2
         (api/input :kn step-1)))
  (is (s/valid? ::st/input step-2)))

(def env-1 (en/add-prefix en/blank-env "ex" "https://example.com/"))

(def step-3
  [st/blank-state
   {::st/event ::st/graph-start
    ::en/env env-1} ; TODO: should be blank?
   {::st/event ::st/prefix
    ::st/input {::st/format :kn
                ::st/line-number 1
                ::st/lines ["@prefix ex: <https://example.com/>"]}
    ::st/prefix "ex"
    ::en/env env-1}
   {::st/event ::st/space
    ::st/input {::st/format :kn
                ::st/line-number 2
                ::st/lines [""]}
    ::en/env env-1}
   {::st/event ::st/subject-start
    ::st/input {::st/format :kn
                ::st/line-number 3
                ::st/lines [": ex:s"]}
    ::en/env env-1
    ::rdf/subject {::rdf/iri "https://example.com/s"}}
   {::st/event ::st/statement
    ::st/input {::st/format :kn
                ::st/line-number 4
                ::st/lines ["ex:p: o"]}
    ::en/env env-1
    ::rdf/graph nil
    ::rdf/subject {::rdf/iri "https://example.com/s"}
    ::rdf/predicate {::rdf/iri "https://example.com/p"}
    ::rdf/object {::rdf/lexical "o"}}
   {::st/event ::st/error
    ::st/input {::st/format :kn
                ::st/line-number 5
                ::st/lines ["foo: bar"]}
    ::en/env env-1
    ::rdf/graph nil
    ::rdf/subject {::rdf/iri "https://example.com/s"}
    ::st/error {::st/error-type :unrecognized-predicate
                ::st/error-info ["foo"]
                ::st/error-message "Unrecognized predicate: foo"}}
   {::st/event ::st/subject-end
    ::en/env env-1
    ::rdf/graph nil
    ::rdf/subject {::rdf/iri "https://example.com/s"}}
   {::st/event ::st/graph-end
    ::en/env env-1
    ::rdf/graph nil}])

(deftest test-step-process-inputs
  (is (= step-3
         (api/run-operations [step-2])))
  (is (s/valid? ::st/states step-3)))

(def step-4
  [st/blank-state
   {::st/event ::st/graph-start
    ::en/env env-1} ; TODO: should be blank?
   {::st/event ::st/prefix
    ::st/input {::st/format :kn
                ::st/line-number 1
                ::st/lines ["@prefix ex: <https://example.com/>"]}
    ::en/env env-1
    ::st/prefix "ex"}
   {::st/event ::st/space
    ::st/input {::st/format :kn
                ::st/line-number 2
                ::st/lines [""]}
    ::en/env env-1}
   {::st/event ::st/subject-start
    ::st/input {::st/format :kn
                ::st/line-number 3
                ::st/lines [": ex:s"]}
    ::en/env env-1
    ::rdf/subject {::rdf/iri "https://example.com/s"}}
   {::st/event ::st/statement
    ::st/input {::st/format :kn
                ::st/line-number 4
                ::st/lines ["ex:p: o"]}
    ::en/env env-1
    ::rdf/graph nil
    ::rdf/subject {::rdf/iri "https://example.com/s"}
    ::rdf/predicate {::rdf/iri "https://example.com/p"}
    ::rdf/object {::rdf/lexical "o"}
    ::st/output {::st/format :nq
                 ::st/line-number 1
                 ::st/lines ["<https://example.com/s> <https://example.com/p> \"o\" ."]}}
   {::st/event ::st/error
    ::st/input {::st/format :kn
                ::st/line-number 5
                ::st/lines ["foo: bar"]}
    ::en/env env-1
    ::rdf/graph nil
    ::rdf/subject {::rdf/iri "https://example.com/s"}
    ::st/error {::st/error-type :unrecognized-predicate
                ::st/error-info ["foo"]
                ::st/error-message "Unrecognized predicate: foo"}}
   {::st/event ::st/subject-end
    ::en/env env-1
    ::rdf/graph nil
    ::rdf/subject {::rdf/iri "https://example.com/s"}}
   {::st/event ::st/graph-end
    ::en/env env-1
    ::rdf/graph nil}])

(deftest test-step-process-output
  (is (= step-4
         (api/run-operations
          [step-2
           {::api/operation-type :render
            ::st/format :nq}])))
  (is (s/valid? ::st/states step-4)))

(def string-output
  "<https://example.com/s> <https://example.com/p> \"o\" .")

(deftest test-content
  (is (= string-output
         (api/content step-4))))

(def errors-string
  "Unrecognized predicate: foo")

(deftest test-errors
  (is (= errors-string
         (api/errors step-4))))

(def operations
  [{::api/operation-type :read-env
    ::st/source "foo.kn"
    ::st/format :kn
    ::st/lines []}
   {::api/operation-type :read
    ::st/source "foo.tsv"
    ::st/format :tsv
    ::st/lines []}
   {::api/operation-type :sort}
   {::api/operation-type :reset-env}
   {::api/operation-type :convert
    ::st/format :nq}])

(deftest test-operations
  (is (s/valid? ::api/operations operations)))
