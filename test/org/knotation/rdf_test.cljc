(ns org.knotation.rdf-test
  (:require [clojure.test :refer :all]
            [org.knotation.rdf :as rdf]))

(deftest test-sequential-blank-nodes
  (->> [{:sb "a" :ob "b"}
        {:sb "b"}
        {:ob "c"}
        {:si "s" :ob "c"}]
       rdf/sequential-blank-nodes
       (= [{:sb "_:b0" :ob "_:b1"}
           {:sb "_:b1"}
           {:ob "_:b2"}
           {:si "s" :ob "_:b2"}])
       is))
