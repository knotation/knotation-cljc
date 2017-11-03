(ns org.knotation.cli-test
  (:require [clojure.test :refer [deftest is testing]]
            [org.knotation.cli :as cli]))

(deftest test-group-args
  #_(is (= (cli/group-args)
           ["foo" "-e" "bar" "baz" "--format=ttl"]
           [{:value "foo"}
            {:flag :env :value "bar"}
            {:value "baz"}
            {:flag :format :value "ttl"}])))
