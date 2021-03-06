(ns org.knotation.nq-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as string]
            [orchestra.spec.test :as stest]

            [org.knotation.rdf :as rdf]
            [org.knotation.state :as st]
            [org.knotation.nq :as nq]
            [org.knotation.nq-spec]))

(stest/instrument)

(deftest test-render-quad
  (is (= (nq/render-quad #::rdf{:si "SI" :pi "PI" :oi "OI"})
         "<SI> <PI> <OI> .\n"))
  (is (= (nq/render-quad #::rdf{:gi "GI" :si "SI" :pi "PI" :oi "OI"})
         "<SI> <PI> <OI> <GI> .\n"))
  (is (= (nq/render-quad #::rdf{:si "SI" :pi "PI" :ob "_:OB"})
         "<SI> <PI> _:OB .\n"))
  (is (= (nq/render-quad #::rdf{:si "SI" :pi "PI" :ol "OL"})
         "<SI> <PI> \"OL\" .\n"))
  (is (= (nq/render-quad #::rdf{:si "SI" :pi "PI" :ol "OL" :lt "en"})
         "<SI> <PI> \"OL\"@en .\n"))
  (is (= (nq/render-quad #::rdf{:si "SI" :pi "PI" :ol "OL" :di "DI"})
         "<SI> <PI> \"OL\"^^<DI> .\n"))
  (is (= (nq/render-quad #::rdf{:si "SI" :pi "PI" :ol "\"OL\""})
         "<SI> <PI> \"\\\"OL\\\"\" .\n"))
  (is (= (nq/render-quad #::rdf{:si "SI" :pi "PI" :ol "O\nL"})
         (str "<SI> <PI> \"" (string/escape "O\nL" char-escape-string) "\" .\n"))))

(deftest test-render-state
  (is (= (nq/render-state
          {::st/event ::st/statement
           ::rdf/quad #::rdf{:gi "GI" :si "SI" :pi "PI" :oi "OI"}})
         {::st/event ::st/statement
          ::rdf/quad #::rdf{:gi "GI" :si "SI" :pi "PI" :oi "OI"}
          ::st/location #::st{:line-number 2 :column-number 1}
          ::st/output
          #::st{:format :nq
                :content "<SI> <PI> <OI> <GI> .\n"
                :start #::st{:line-number 1 :column-number 1}
                :end #::st{:line-number 2 :column-number 0}}})))
