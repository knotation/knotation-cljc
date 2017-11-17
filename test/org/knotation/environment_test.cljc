(ns org.knotation.environment-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [org.knotation.environment-spec]
            [org.knotation.environment :as en]))

(stest/instrument)

(deftest test-environments
  (is (s/valid? ::en/env en/blank-env))
  (is (s/valid? ::en/env en/default-env)))

(deftest test-add-prefix
  (is (= {::en/prefix-iri {"ex" "https://example.com/"}
          ::en/iri-prefix {"https://example.com/" "ex"}
          ::en/prefix-seq ["ex"]}
         (en/add-prefix {} "ex" "https://example.com/"))))

(deftest test-add-label
  (is (= {::en/label-iri {"Foo" "https://example.com/foo"}
          ::en/iri-label {"https://example.com/foo" "Foo"}
          ::en/label-seq ["Foo"]}
         (en/add-label {} "Foo" "https://example.com/foo"))))

(deftest test-set-datatype
  (is (= {::en/predicate-datatype {"https://example.com/p" "https://example.com/d"}}
         (en/set-datatype {} "https://example.com/p" "https://example.com/d"))))

(deftest test-set-language
  (is (= {::en/predicate-language {"https://example.com/p" "@en"}}
         (en/set-language {} "https://example.com/p" "@en"))))
