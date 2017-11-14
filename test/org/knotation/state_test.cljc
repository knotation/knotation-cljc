(ns org.knotation.state-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]
            [org.knotation.state-spec :as sts]))

(stest/instrument)

(deftest test-examples
  (is (s/valid? ::st/state st/example-quads))
  (is (s/valid? ::st/input-state st/example-quads))
  (is (s/valid? ::st/output-state st/example-quads))
  (is (s/valid? ::st/state st/example-error)))

(deftest test-states
  (is (s/valid? ::st/state st/blank-state))
  (is (s/valid? ::st/state st/default-state)))
