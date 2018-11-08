(ns org.knotation.state-test
  (:require [clojure.test :refer [deftest is testing]]
            [orchestra.spec.test :as stest]

            [org.knotation.environment :as en]
            [org.knotation.state :as st]
            [org.knotation.state-spec]))

(stest/instrument)

(def example-1
  [{::st/event ::st/prefix
    ::st/prefix "ex"
    ::st/iri "http://example.com/"}])
