(ns org.knotation.omn-test
  (:require [clojure.test :refer [deftest is testing]]
            [#?(:clj clojure.spec.alpha :cljs cljs.spec.alpha) :as s]
            [#?(:clj orchestra.spec.test :cljs cljs.spec.test.alpha) :as stest]
            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]
            [org.knotation.omn :as omn]))

(stest/instrument)

(def ex-env
  (-> en/blank-env
      (en/add-label "foo" (rdf/ex "foo"))
      (en/add-label "bar" (rdf/ex "bar"))
      (en/add-label "foo bar" (rdf/ex "foo-bar"))
      (en/add-label "has part" (rdf/ex "has-part"))
      (en/add-label "is about" (rdf/ex "is-about"))
      (en/add-label "has_specified_output" (rdf/ex "has_specified_output"))
      (en/add-label "material entity" (rdf/ex "material-entity"))
      (en/add-label "information content entity" (rdf/ex "information-content-entity"))
      (en/add-label "has role" (rdf/ex "has-role"))
      (en/add-label "evaluant role" (rdf/ex "evaluant-role"))))

(deftest test-manchester-parsing
  (testing "Simple label"
    (is (= (omn/parse-class-expression "foo")
           [:CLASS_EXPRESSION [:LABEL "" "foo" ""]])))

  (testing "Quoted label"
    (is (= (omn/parse-class-expression "'foo'")
           [:CLASS_EXPRESSION [:LABEL "'" "foo" "'"]])))

  (testing "Parens"
    (is (= (omn/parse-class-expression "(foo )")
           [:CLASS_EXPRESSION
            "(" [:CLASS_EXPRESSION [:LABEL "" "foo" ""]] " " ")"])))

  (testing "Disjunction"
    (is (= (omn/parse-class-expression "foo or bar")
           [:CLASS_EXPRESSION
            [:DISJUNCTION
             [:CLASS_EXPRESSION [:LABEL "" "foo" ""]]
             " " "or" " "
             [:CLASS_EXPRESSION [:LABEL "" "bar" ""]]]])))

  (testing "Conjunction"
    (is (= (omn/parse-class-expression "foo and bar")
           [:CLASS_EXPRESSION
            [:CONJUNCTION [:CLASS_EXPRESSION [:LABEL "" "foo" ""]]
             " " "and" " "
             [:CLASS_EXPRESSION [:LABEL "" "bar" ""]]]])))

  (testing "Negation"
    (is (= (omn/parse-class-expression "not foo")
           [:CLASS_EXPRESSION
            [:NEGATION
             "not" " "
             [:CLASS_EXPRESSION
              [:LABEL "" "foo" ""]]]])))

  (testing "Some"
    (is (= (omn/parse-class-expression "'has part' some foo")
           [:CLASS_EXPRESSION
            [:SOME
             [:OBJECT_PROPERTY_EXPRESSION
              [:LABEL "'" "has part" "'"]]
             " " "some" " "
             [:CLASS_EXPRESSION [:LABEL "" "foo" ""]]]])))

  (testing "Some not"
    (is (= (omn/parse-class-expression "'has part' some not foo")
           [:CLASS_EXPRESSION
            [:SOME
             [:OBJECT_PROPERTY_EXPRESSION
              [:LABEL "'" "has part" "'"]]
             " " "some" " "
             [:CLASS_EXPRESSION
              [:NEGATION
               "not" " "
               [:CLASS_EXPRESSION
                [:LABEL "" "foo" ""]]]]]])))

  (testing "Complex axiom"
    (is (= (omn/parse-class-expression "'is about' some
    ('material entity'
     and ('has role' some 'evaluant role'))")
           [:CLASS_EXPRESSION
            [:SOME
             [:OBJECT_PROPERTY_EXPRESSION [:LABEL "'" "is about" "'"]]
             " " "some" "\n    "
             [:CLASS_EXPRESSION
              "("
              [:CLASS_EXPRESSION
               [:CONJUNCTION
                [:CLASS_EXPRESSION
                 [:LABEL "'" "material entity" "'"]]
                "\n     " "and" " "
                [:CLASS_EXPRESSION
                 "("
                 [:CLASS_EXPRESSION
                  [:SOME
                   [:OBJECT_PROPERTY_EXPRESSION
                    [:LABEL "'" "has role" "'"]]
                   " " "some" " "
                   [:CLASS_EXPRESSION
                    [:LABEL "'" "evaluant role" "'"]]]]
                 ")"]]]
              ")"]]])))

  (testing "Another complex axiom"
    (is (= (omn/parse-class-expression
            "has_specified_output some
('information content entity'
 and ('is about' some
    ('material entity'
     and ('has role' some 'evaluant role'))))")
           [:CLASS_EXPRESSION
            [:SOME
             [:OBJECT_PROPERTY_EXPRESSION
              [:LABEL "" "has_specified_output" ""]]
             " " "some" "\n"
             [:CLASS_EXPRESSION
              "("
              [:CLASS_EXPRESSION
               [:CONJUNCTION
                [:CLASS_EXPRESSION
                 [:LABEL "'" "information content entity" "'"]]
                "\n " "and" " "
                [:CLASS_EXPRESSION
                 "("
                 [:CLASS_EXPRESSION
                  [:SOME
                   [:OBJECT_PROPERTY_EXPRESSION
                    [:LABEL "'" "is about" "'"]]
                   " " "some" "\n    "
                   [:CLASS_EXPRESSION
                    "("
                    [:CLASS_EXPRESSION
                     [:CONJUNCTION
                      [:CLASS_EXPRESSION
                       [:LABEL
                        "'" "material entity" "'"]]
                      "\n     " "and" " "
                      [:CLASS_EXPRESSION
                       "("
                       [:CLASS_EXPRESSION
                        [:SOME
                         [:OBJECT_PROPERTY_EXPRESSION
                          [:LABEL
                           "'" "has role" "'"]]
                         " " "some" " "
                         [:CLASS_EXPRESSION
                          [:LABEL "'" "evaluant role" "'"]]]]
                       ")"]]]
                    ")"]]]
                 ")"]]]
              ")"]]]))))

(defn reads-to?
  [string maps]
  (= (rdf/sequential-blank-nodes maps)
     (rdf/sequential-blank-nodes (omn/read-class-string ex-env string))))

(deftest test-class-expression-readers
  (is (reads-to?
       "foo"
       [#::rdf{:oi "http://example.com/foo"}]))
  (is (reads-to?
       "not foo"
       [#::rdf{:ob "_:b0"}
        #::rdf{:sb "_:b0" :pi (rdf/rdf "type") :oi (rdf/owl "Class")}
        #::rdf{:sb "_:b0" :pi (rdf/owl "complementOf") :oi "http://example.com/foo"}]))
  (is (reads-to?
       "foo or bar"
       [#::rdf{:ob "_:b0"}
        #::rdf{:sb "_:b0" :pi (rdf/rdf "type") :oi (rdf/owl "Class")}
        #::rdf{:sb "_:b0" :pi (rdf/rdf "unionOf") :ob "_:b1"}
        #::rdf{:sb "_:b1" :pi (rdf/rdf "first") :oi "http://example.com/foo"}
        #::rdf{:sb "_:b1" :pi (rdf/rdf "rest") :ob "_:b2"}
        #::rdf{:sb "_:b2" :pi (rdf/rdf "first") :oi "http://example.com/bar"}
        #::rdf{:sb "_:b2" :pi (rdf/rdf "rest") :oi (rdf/rdf "nil")}]))
  (is (reads-to?
       "'has part' some foo"
       [#::rdf{:ob "_:b0"}
        #::rdf{:sb "_:b0" :pi (rdf/rdf "type") :oi (rdf/owl "Restriction")}
        #::rdf{:sb "_:b0" :pi (rdf/owl "onProperty") :oi "http://example.com/has-part"}
        #::rdf{:sb "_:b0" :pi (rdf/owl "someValuesFrom") :oi "http://example.com/foo"}]))
  (is (reads-to?
       "'has part' some (foo or bar)"
       [#::rdf{:ob "_:b0"}
        #::rdf{:sb "_:b0" :pi (rdf/rdf "type") :oi (rdf/owl "Restriction")}
        #::rdf{:sb "_:b0" :pi (rdf/owl "onProperty") :oi "http://example.com/has-part"}
        #::rdf{:sb "_:b0" :pi (rdf/owl "someValuesFrom") :ob "_:b1"}
        #::rdf{:sb "_:b1" :pi (rdf/rdf "type") :oi (rdf/owl "Class")}
        #::rdf{:sb "_:b1" :pi (rdf/rdf "unionOf") :ob "_:b2"}
        #::rdf{:sb "_:b2" :pi (rdf/rdf "first") :oi "http://example.com/foo"}
        #::rdf{:sb "_:b2" :pi (rdf/rdf "rest") :ob "_:b3"}
        #::rdf{:sb "_:b3" :pi (rdf/rdf "first") :oi "http://example.com/bar"}
        #::rdf{:sb "_:b3" :pi (rdf/rdf "rest") :oi (rdf/rdf "nil")}])))

(defn render-expression
  [s]
  (->> s
       omn/parse-class-expression
       (omn/read-class-expression ex-env)
       rdf/sequential-blank-nodes
       (#(concat [(merge #::rdf{:si "s" :pi "p"} (first %))]
                 (rest %)))
       (map #(assoc st/default-state
                    ::st/event ::st/statement
                    ::en/env ex-env
                    ::rdf/subject (or (::rdf/si %) (::rdf/sb %))
                    ::rdf/quad %))
       omn/render-states))

(defn test-round-trip
  [s]
  (->> s
       render-expression
       (= s)
       is))

(deftest test-round-trips
  (test-round-trip "foo")
  (test-round-trip "'foo bar'")
  (test-round-trip "not foo")
  (test-round-trip "foo or bar")
  (test-round-trip "foo or foo or foo")
  (test-round-trip "foo and bar")
  (test-round-trip "not (foo or bar)")
  (test-round-trip "'has part' some foo")
  (test-round-trip "'has part' only foo")
  (test-round-trip "'has part' some (foo or bar)")
  (test-round-trip "'is about' some ('material entity' and 'has role' some 'evaluant role')")
  (test-round-trip "has_specified_output some ('information content entity' and 'is about' some ('material entity' and 'has role' some 'evaluant role'))"))
