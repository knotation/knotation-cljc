(ns org.knotation.api-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
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
  {::st/format :kn
   ::st/line-number 1
   ::st/lines (clojure.string/split-lines step-1)})

(deftest test-kn
  (is (= step-2
         (api/kn step-1)))
  (is (s/valid? ::st/input step-2)))

(def step-3
  [{::st/format :kn
    ::st/line-number 1
    ::st/lines ["@prefix ex: <https://example.com/>"]}
   {::st/format :kn
    ::st/line-number 2
    ::st/lines [""]}
   {::st/format :kn
    ::st/line-number 3
    ::st/lines [": ex:s"]}
   {::st/format :kn
    ::st/line-number 4
    ::st/lines ["ex:p: o"]}
   {::st/format :kn
    ::st/line-number 5
    ::st/lines ["foo: bar"]}])

(deftest test-group-lines
  (is (= step-3
         (api/group-lines step-2)))
  (is (s/valid? (s/coll-of ::st/input) step-3)))

(def env-1 (en/add-prefix en/blank-env "ex" "https://example.com/"))

(def step-4
  [{::st/input {::st/format :kn
                ::st/line-number 1
                ::st/lines ["@prefix ex: <https://example.com/>"]}
    ::en/env-before {}
    ::en/env env-1}
   {::st/input {::st/format :kn
                ::st/line-number 2
                ::st/lines [""]}
    ::en/env-before env-1
    ::en/env env-1}
   {::st/input {::st/format :kn
                ::st/line-number 3
                ::st/lines [": ex:s"]}
    ::en/env-before env-1
    ::en/env env-1
    ::rdf/subject {::rdf/iri "https://example.com/s"}}
   {::st/input {::st/format :kn
                ::st/line-number 4
                ::st/lines ["ex:p: o"]}
    ::en/env-before env-1
    ::en/env env-1
    ::rdf/subject {::rdf/iri "https://example.com/s"}
    ::rdf/quads [{::rdf/graph nil
                  ::rdf/subject {::rdf/iri "https://example.com/s"}
                  ::rdf/predicate {::rdf/iri "https://example.com/p"}
                  ::rdf/object {::rdf/lexical "o"}}]}
   {::st/input {::st/format :kn
                ::st/line-number 5
                ::st/lines ["foo: bar"]}
    ::en/env-before env-1
    ::en/env env-1
    ::rdf/subject {::rdf/iri "https://example.com/s"}
    ::st/error {::st/error-type :unrecognized-predicate
                ::st/error-message "Unrecognized predicate: foo"}}])

(deftest test-step-process-inputs
  (is (= step-4
         (api/process-inputs {::api/inputs [step-2]})))
  (is (s/valid? (s/coll-of ::st/input-state) step-4)))

(def step-5
  [{::st/input {::st/format :kn
                ::st/line-number 1
                ::st/lines ["@prefix ex: <https://example.com/>"]}
    ::en/env-before {}
    ::en/env env-1
    ::st/output-line-count 0}
   {::st/input {::st/format :kn
                ::st/line-number 2
                ::st/lines [""]}
    ::en/env-before env-1
    ::en/env env-1
    ::st/output-line-count 0}
   {::st/input {::st/format :kn
                ::st/line-number 3
                ::st/lines [": ex:s"]}
    ::en/env-before env-1
    ::en/env env-1
    ::rdf/subject {::rdf/iri "https://example.com/s"}
    ::st/output-line-count 0}
   {::st/input {::st/format :kn
                ::st/line-number 4
                ::st/lines ["ex:p: o"]}
    ::en/env-before env-1
    ::en/env env-1
    ::rdf/subject {::rdf/iri "https://example.com/s"}
    ::rdf/quads [{::rdf/graph nil
                  ::rdf/subject {::rdf/iri "https://example.com/s"}
                  ::rdf/predicate {::rdf/iri "https://example.com/p"}
                  ::rdf/object {::rdf/lexical "o"}}]
    ::st/output-line-count 1
    ::st/output {::st/format :nq
                 ::st/line-number 1
                 ::st/lines ["<https://example.com/s> <https://example.com/p> \"o\" ."]}}
   {::st/input {::st/format :kn
                ::st/line-number 5
                ::st/lines ["foo: bar"]}
    ::en/env-before env-1
    ::en/env env-1
    ::rdf/subject {::rdf/iri "https://example.com/s"}
    ::st/output-line-count 1
    ::st/error {::st/error-type :unrecognized-predicate
                ::st/error-message "Unrecognized predicate: foo"}}])

(deftest test-step-process-output
  (is (= step-5
         (api/process-output
          {::api/output {::st/format :nq}}
          step-4)))
  (is (s/valid? (s/coll-of ::st/output-state) step-5)))

(def string-output
  "<https://example.com/s> <https://example.com/p> \"o\" .")

(deftest test-content
  (is (= string-output
         (api/content step-5))))

(def errors-string
  "Unrecognized predicate: foo")

(deftest test-errors
  (is (= errors-string
         (api/errors step-5))))
