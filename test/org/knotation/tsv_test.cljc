(ns org.knotation.tsv-test
  (:require [clojure.test :refer [deftest is testing]]
            [org.knotation.state :as st]
            [org.knotation.tsv :as tsv]))

(def example-state-1
  (-> st/default-state
      (st/add-prefix "ex" "https://example.com/")
      (st/add-label "homepage" "https://example.com/homepage")
      (st/add-datatype "https://example.com/homepage" "https://knotation.org/datatype/link")))

(def example-columns-1
  [{:column-number 1
    :label "@subject"
    :subject? true}
   {:column-number 2
    :label "label"
    :predicate {:iri "http://www.w3.org/2000/01/rdf-schema#label"}}
   {:column-number 3
    :label "homepage"
    :predicate {:iri "https://example.com/homepage"}}])

(defn test-line
  [line before after-fn]
  (is (= (tsv/block->state (assoc before :block [line]))
         (assoc (after-fn before) :block [line]))
      (str "Testing line: " line)))

(deftest test-tsv
  (test-line
   "@subject	label	homepage"
   example-state-1
   #(assoc % :columns example-columns-1))

  (test-line
   "ex:foo	Foo	https://example.com"
   (assoc example-state-1 :columns example-columns-1)
   #(-> %
        (st/add-label "Foo" "https://example.com/foo")
        (assoc
         :subject {:iri "https://example.com/foo"}
         :quads
         [{:graph nil
           :subject {:iri "https://example.com/foo"}
           :predicate {:iri "http://www.w3.org/2000/01/rdf-schema#label"}
           :object {:lexical "Foo"}}
          {:graph nil
           :subject {:iri "https://example.com/foo"}
           :predicate {:iri "https://example.com/homepage"}
           :object {:iri "https://example.com"}}]))))
