(ns org.knotation.environment-test
  (:require [clojure.test :refer [deftest is testing]]
            [orchestra.spec.test :as stest]

            [org.knotation.environment :as en]
            [org.knotation.environment-spec]))

(stest/instrument)

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
