(ns org.knotation.api-test
  (:require [clojure.test :refer [deftest is testing]]
            [org.knotation.state :as st]
            [org.knotation.api :as api]))

(def step-1
  "@prefix ex: <https://example.com/>

: ex:s
ex:p: \"o\"
foo: bar")

(def step-2
  {:format :kn
   :lines (clojure.string/split-lines step-1)})

(def step-3
  [{:format :kn
    :line-number 1
    :lines ["@prefix ex: <https://example.com/>"]}
   {:format :kn
    :line-number 2
    :lines [""]}
   {:format :kn
    :line-number 3
    :lines [": ex:s"]}
   {:format :kn
    :line-number 4
    :lines ["ex:p: \"o\""]}
   {:format :kn
    :line-number 5
    :lines ["foo: bar"]}])

(def env-1 (st/add-prefix {} "ex" "https://example.com"))
(def env-2 (assoc env-1 :subject {:iri "https://example.com/s"}))

(def step-4
  [{:input {:format :kn
            :line-number 1
            :lines ["@prefix ex: <https://example.com/>"]}
    :env-before nil
    :env env-1}
   {:input {:format :kn
            :line-number 2
            :lines [""]}
    :env-before env-1
    :env env-1}
   {:input {:format :kn
            :line-number 3
            :lines [": ex:s"]}
    :env-before env-1
    :env env-2}
   {:input {:format :kn
            :line-number 4
            :lines ["ex:p: \"o\""]}
    :env-before env-2
    :env env-2
    :quads [{:graph nil
             :subject {:iri "https://example.com/s"}
             :predicate {:iri "https://example.com/p"}
             :object {:lexical "o"}}]}
   {:input {:format :kn
            :line-number 5
            :lines ["foo: bar"]}
    :env-before env-2
    :env env-2
    :error {:message "Unrecognized predicate: foo"
            :error-type :unrecognized-predicate}}])

(def step-5
  [{:input {:format :kn
            :line-number 1
            :lines ["@prefix ex: <https://example.com/>"]}
    :env-before nil
    :env env-1}
   {:input {:format :kn
            :line-number 2
            :lines [""]}
    :env-before env-1
    :env env-1}
   {:input {:format :kn
            :line-number 3
            :lines [": ex:s"]}
    :env-before env-1
    :env env-2}
   {:input {:format :kn
            :line-number 4
            :lines ["ex:p: \"o\""]}
    :env-before env-2
    :env env-2
    :quads [{:graph nil
             :subject {:iri "https://example.com/s"}
             :predicate {:iri "https://example.com/p"}
             :object {:lexical "o"}}]
    :output {:format :nq
             :lines ["<https://example.com/s> <https://example.com/p> \"o\" ."]}}
   {:input {:format :kn
            :line-number 5
            :lines ["foo: bar"]}
    :env-before env-2
    :env env-2
    :error {:message "Unrecognized predicate: foo"
            :error-type :unrecognized-predicate}}])

(def string-output
  "<https://example.com/s> <https://example.com/p> \"o\" .")
