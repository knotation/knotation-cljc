(ns org.knotation.cli-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [org.knotation.state :as st]
            [org.knotation.api :as api]
            [org.knotation.cli :as cli]))

(stest/instrument)

(deftest test-group-args
  (is (= [{::cli/value "foo"}
          {::cli/flag :env ::cli/value "bar"}
          {::cli/value "baz"}
          {::cli/flag :reset-env}
          {::cli/flag :format ::cli/value "ttl"}
          {::cli/flag :format ::cli/value "nq"}]
         (cli/group-args
          ["foo" "-e" "bar" "baz" "--reset-env" "--format=ttl" "-f" "nq"]))))

(deftest test-operations
  (with-redefs [line-seq (fn [x] [])
                clojure.java.io/reader (fn [x] nil)]
    (is (= [{::api/operation-type :read-env
             ::st/source "foo.kn"
             ::st/format :kn
             ::st/lines []}
            {::api/operation-type :read
             ::st/source "foo.tsv"
             ::st/format :tsv
             ::st/lines []}
            {::api/operation-type :sort}
            {::api/operation-type :reset-env}
            {::api/operation-type :render
             ::st/format :nq}]
           (cli/operations
            ["-e" "foo.kn" "foo.tsv" "--sort" "--reset-env" "--format=nq"])))))
