(ns org.knotation.cli-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [org.knotation.cli :as cli]))

(stest/instrument)

;(deftest test-group-args
;  (is (= (cli/group-args
;           ["foo" "-e" "bar" "baz" "--format=ttl"]
;           [{:value "foo"}
;            {:flag :env :value "bar"}
;            {:value "baz"}
;            {:flag :format :value "ttl"}]))))
