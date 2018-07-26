(ns org.knotation.kn-test
  (:require [clojure.test :refer [deftest is testing]]
            [org.knotation.util :as util]
            [org.knotation.environment :as en]
            [org.knotation.format :as fm]
            [org.knotation.kn :as kn]
            [org.knotation.api :as api]))

(defn test-roundtrip-line
  [parse-fn read-fn render-fn line]
  (->> line
       parse-fn
       (read-fn nil)
       (render-fn nil)
       fm/render-parses
       (= line)
       is))

(defn normalize-trailing-newlines [s]
  (str (clojure.string/trim s) "\n"))

(defn test-before-after
  [before after]
  (->> before
       util/split-lines
       (fm/read-lines :kn en/blank-env)
       (fm/render-states :kn en/blank-env)
       fm/render-output
       normalize-trailing-newlines
       (= (normalize-trailing-newlines after))
       is))

(defn test-roundtrip
  [content]
  (test-before-after content content))

(deftest test-roundtrip-lines
  (test-roundtrip-line kn/parse-blank kn/read-blank kn/render-blank "\n")
  (test-roundtrip-line kn/parse-comment kn/read-comment kn/render-comment "# Foo \n")
  (test-roundtrip-line kn/parse-prefix kn/read-prefix kn/render-prefix "@prefix foo: <bar>\n")
  (test-roundtrip-line kn/parse-subject kn/read-subject kn/render-subject ": <bar>\n")
  ;(println (->> "<foo>: bar\n" kn/parse-statement (kn/read-statement nil) (kn/render-statement nil)))
  ;(println (->> "<foo>; <bat>: bar\n" kn/parse-statement (kn/read-statement nil)))
  ;(println (->> "<foo>; @en: bar\n" kn/parse-statement (kn/read-statement nil)))
  (test-roundtrip-line kn/parse-statement kn/read-statement kn/render-statement "<foo>: bar\n")
  (test-roundtrip-line kn/parse-statement kn/read-statement kn/render-statement "<foo>; <bat>: bar\n")
  (test-roundtrip-line kn/parse-statement kn/read-statement kn/render-statement "<foo>; @en: bar\n"))

(deftest test-rountrips
  (test-roundtrip
   "@prefix ex: <http://example.com/>

# comment

: ex:s
ex:p; ex:d: Multiline
 string
 
  with spaces.
"))

(deftest test-templates
  (test-before-after
   "@prefix knp: <https://knotation.org/predicate/>
@prefix ex: <http://example.com/>

: ex:template
knp:template-content: 
 ex:label: Foo {label}

: ex:1
ex:foo: bar
knp:apply-template: ex:template
 label: Bar"
   "@prefix knp: <https://knotation.org/predicate/>
@prefix ex: <http://example.com/>

: ex:template
knp:template-content: 
 ex:label: Foo {label}

: ex:1
ex:foo: bar
knp:applied-template: ex:template
 label: Bar
ex:label: Foo Bar
"))

(deftest test-annotations
  (testing "Annotations with no valid targets show up as errors"
    (let [res (->> "@prefix ex: <http://example.com/>

> ex:p: This annotation has no target and should therefore error"
                   util/split-lines
                   (fm/read-lines :kn en/blank-env))]
      (is (api/any-errors? res))
      (is (->> (nth res 3) api/error-type (= :no-annotation-target)))))
  (testing "Annotations start with pointies"
    (test-roundtrip "@prefix ex: <http://example.com/>

: ex:s
ex:p: ex:o
> ex:a: ex:b"))
  (testing "Annotations can refer to other annotations"
    (let [chained "@prefix ex: <http://example.com/>

: ex:s
ex:p: ex:o
> ex:a: ex:b
>> ex:c: ex:d"
          res (->> chained util/split-lines
                   (fm/read-lines :kn en/blank-env))]
      (test-roundtrip chained)
      (is (= {:si "http://example.com/s", :pi "http://example.com/p", :ol "ex:o"}
             (:target (nth res 5))))
      (is (= {:si "http://example.com/s", :pi "http://example.com/a", :ol "ex:b"}
             (:target (nth res 11))))))
  (testing "Non-adjacent annotations can refer to previous annotation targets"
    (let [deep-chain "@prefix ex: <http://example.com/>

: ex:s
ex:p: A
> ex:p: B is an annotation on A
>> ex:p: C is an annotation on B
> ex:p: D is an annotation on A"
          res (->> deep-chain util/split-lines
                   (fm/read-lines :kn en/blank-env))]
      (test-roundtrip deep-chain)
      (is (= {:si "http://example.com/s", :pi "http://example.com/p", :ol "A"}
             (:target (nth res 5))))
      (is (= {:si "http://example.com/s", :pi "http://example.com/p", :ol "B is an annotation on A"}
             (:target (nth res 11))))
      (is (= {:si "http://example.com/s", :pi "http://example.com/p", :ol "A"}
             (:target (nth res 17))))))
  (testing "Annotations can have multi-line strings"
    (test-roundtrip "@prefix ex: <http://example.com/>

: ex:s
ex:p: A
> ex:p: B is an annotation on A
  that includes a multi-line string
>> ex:p: C is an annotation on B
   that likewise includes a multi-line string
> ex:p: D is an annotation on A")))

(deftest test-manchester-parsing
  (testing "Simple label"
    (is (= (kn/parse-manchester "foo")
           [:CLASS_EXPRESSION [:LABEL "" "foo" ""]])))

  (testing "Quoted label"
    (is (= (kn/parse-manchester "'foo'")
           [:CLASS_EXPRESSION [:LABEL "'" "foo" "'"]])))

  (testing "Parens"
    (is (= (kn/parse-manchester "(foo )")
           [:CLASS_EXPRESSION
            "(" [:CLASS_EXPRESSION [:LABEL "" "foo" ""]] " " ")"])))

  (testing "Disjunction"
    (is (= (kn/parse-manchester "foo or bar")
           [:CLASS_EXPRESSION
            [:DISJUNCTION
             [:CLASS_EXPRESSION [:LABEL "" "foo" ""]]
             " " "or" " "
             [:CLASS_EXPRESSION [:LABEL "" "bar" ""]]]])))

  (testing "Conjunction"
    (is (= (kn/parse-manchester "foo and bar")
           [:CLASS_EXPRESSION
            [:CONJUNCTION [:CLASS_EXPRESSION [:LABEL "" "foo" ""]]
             " " "and" " "
             [:CLASS_EXPRESSION [:LABEL "" "bar" ""]]]])))

  (testing "Negation"
    (is (= (kn/parse-manchester "not foo")
           [:CLASS_EXPRESSION
            [:NEGATION
             "not" " "
             [:LABEL "" "foo" ""]]])))

  (testing "Some"
    (is (= (kn/parse-manchester "'has part' some foo")
           [:CLASS_EXPRESSION
            [:SOME
             [:OBJECT_PROPERTY_EXPRESSION
              [:LABEL "'" "has part" "'"]]
             " " "some" " "
             [:CLASS_EXPRESSION [:LABEL "" "foo" ""]]]])))

  (testing "Some not"
    (is (= (kn/parse-manchester "'has part' some not foo")
           [:CLASS_EXPRESSION
            [:SOME
             [:OBJECT_PROPERTY_EXPRESSION
              [:LABEL "'" "has part" "'"]]
             " " "some" " "
             [:CLASS_EXPRESSION
              [:NEGATION
               "not" " "
               [:LABEL "" "foo" ""]]]]])))

  (testing "Complex axiom"
    (is (= (kn/parse-manchester "'is about' some
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
    (is (= (kn/parse-manchester
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
