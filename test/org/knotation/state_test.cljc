(ns org.knotation.state-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.spec.test.alpha :as stest]

            [org.knotation.environment :as en]
            [org.knotation.state :as st]
            [org.knotation.state-spec]))

(stest/instrument)
