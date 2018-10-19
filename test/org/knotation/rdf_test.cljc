(ns org.knotation.rdf-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [org.knotation.rdf :as rdf]
            [org.knotation.rdf-spec]))

(stest/instrument)

(deftest test-sequential-blank-nodes
  (->> [#::rdf{:sb "_:a" :pi "p" :ob "_:b"}
        #::rdf{:sb "_:b" :pi "p"}
        #::rdf{:pi "p" :ob "_:c"}
        #::rdf{:si "s" :pi "p" :ob "_:c"}]
       rdf/sequential-blank-nodes
       (= [#::rdf{:sb "_:b0" :pi "p" :ob "_:b1"}
           #::rdf{:sb "_:b1" :pi "p"}
           #::rdf{:pi "p" :ob "_:b2"}
           #::rdf{:si "s" :pi "p" :ob "_:b2"}])
       is))
