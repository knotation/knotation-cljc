(ns org.knotation.kn-test
  (:require [clojure.test :refer [deftest is testing]]
            [org.knotation.kn :as kn]))

(def example-state-1
  {:env {:prefix-iri {"ex" "https://example.com/"}
         :iri-prefix {"https://example.com/" "ex"}}})

(def example-state-2
  {:env {:label-iri {"label" "https://example.com/label"}
         :iri-label {"https://example.com/label" "label"}}
   :subject {:iri "https://example.com/foo"}})

(def example-state-3
  {:env {:label-iri {"homepage" "https://example.com/homepage"}
         :iri-label {"https://example.com/homepage" "homepage"}
         :iri-datatype {"https://example.com/homepage" "https://knotation.org/format/link"}}
   :subject {:iri "https://example.com/foo"}})

(deftest test-line->state
  (is (= (kn/line->state {} "@prefix ex: <https://example.com/>")
         example-state-1))
  (is (= (kn/line->state example-state-1 ": ex:foo")
         (assoc example-state-1 :subject {:iri "https://example.com/foo"})))
  (is (= (kn/line->state
          (assoc example-state-1 :subject {:iri "https://example.com/foo"})
          "ex:label: Foo")
         (assoc
          example-state-1
          :subject
          {:iri "https://example.com/foo"}
          :quads
          [{:graph nil
            :subject {:iri "https://example.com/foo"}
            :predicate {:iri "https://example.com/label"}
            :object {:lexical "Foo"}}])))
  (is (= (kn/line->state example-state-2 "label: Foo")
         (assoc
          example-state-2
          :quads
          [{:graph nil
            :subject {:iri "https://example.com/foo"}
            :predicate {:iri "https://example.com/label"}
            :object {:lexical "Foo"}}])))
  (is (= (kn/line->state example-state-3 "homepage: http://example.com")
         (assoc
          example-state-3
          :quads
          [{:graph nil
            :subject {:iri "https://example.com/foo"}
            :predicate {:iri "https://example.com/homepage"}
            :object {:iri "http://example.com"}}]))))
