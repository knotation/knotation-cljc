(ns org.knotation.omn-test
  (:require [clojure.test :refer [deftest is testing]]
            [#?(:clj clojure.spec.alpha :cljs cljs.spec.alpha) :as s]
            [#?(:clj orchestra.spec.test :cljs cljs.spec.test.alpha) :as stest]
            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.omn :as omn]))

(stest/instrument)

(def env-1
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
             [:LABEL "" "foo" ""]]])))

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
               [:LABEL "" "foo" ""]]]]])))

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

(defn -intern-slot!
  [atm state key]
  (let [blank (get state key)]
    (assoc
     state key
     (or (get @atm blank)
         (get (swap! atm #(assoc % blank (str (count %)))) blank)))))

(defn replace-blanks
  [states]
  (map
   (let [bs (atom {})]
     (fn [s]
       (let [o (if (:ob s) (-intern-slot! bs s :ob) s)]
         (if (:sb o) (-intern-slot! bs o :sb) o))))
   states))

(defn reads-to?
  [string maps]
  (= (replace-blanks maps)
     (replace-blanks (omn/read-class-string env-1 string))))

(deftest test-class-expression-readers
  (is (reads-to?
       "not foo"
       [{:sb "a" :pi (rdf/rdf "type") :oi (rdf/owl "Class")}
        {:sb "a" :pi (rdf/owl "complementOf") :oi "http://example.com/foo"}]))
  (is (reads-to?
       "foo or bar"
       [{:sb "a" :pi (rdf/rdf "type") :oi (rdf/owl "Class")}
        {:sb "a" :pi (rdf/rdf "unionOf") :ob "b"}
        {:sb "b" :pi (rdf/rdf "first") :oi "http://example.com/foo"}
        {:sb "b" :pi (rdf/rdf "rest") :ob "c"}
        {:sb "c" :pi (rdf/rdf "first") :oi "http://example.com/bar"}
        {:sb "c" :pi (rdf/rdf "rest") :oi (rdf/rdf "nil")}]))
  (is (reads-to?
       "'has part' some foo"
       [{:sb "a" :pi (rdf/rdf "type") :oi (rdf/owl "Restriction")}
        {:sb "a" :pi (rdf/owl "onProperty") :oi "http://example.com/has-part"}
        {:sb "a" :pi (rdf/owl "someValuesFrom") :oi "http://example.com/foo"}]))
  (is (reads-to?
       "'has part' some (foo or bar)"
       [{:sb "a" :pi (rdf/rdf "type") :oi (rdf/owl "Restriction")}
        {:sb "a" :pi (rdf/owl "onProperty") :oi "http://example.com/has-part"}
        {:sb "a" :pi (rdf/owl "someValuesFrom") :ob "b"}
        {:sb "b" :pi (rdf/rdf "type") :oi (rdf/owl "Class")}
        {:sb "b" :pi (rdf/rdf "unionOf") :ob "c"}
        {:sb "c" :pi (rdf/rdf "first") :oi "http://example.com/foo"}
        {:sb "c" :pi (rdf/rdf "rest") :ob "d"}
        {:sb "d" :pi (rdf/rdf "first") :oi "http://example.com/bar"}
        {:sb "d" :pi (rdf/rdf "rest") :oi (rdf/rdf "nil")}])))

;; (def ex-list-branch
;;   [{::rdf/subject {::rdf/bnode "_:1"}
;;     ::rdf/predicate {::rdf/iri (rdf/rdf "first")}
;;     ::rdf/object {::rdf/literal "1"}}
;;    {::rdf/subject {::rdf/bnode "_:1"}
;;     ::rdf/predicate {::rdf/iri (rdf/rdf "rest")}
;;     ::rdf/object
;;     {::rdf/bnode "_:2"
;;      ::rdf/pairs
;;      [{::rdf/predicate {::rdf/iri (rdf/rdf "first")}
;;        ::rdf/object {::rdf/literal "2"}}
;;       {::rdf/predicate {::rdf/iri (rdf/rdf "rest")}
;;        ::rdf/object
;;        {::rdf/bnode "_:3"
;;         ::rdf/pairs
;;         [{::rdf/predicate {::rdf/iri (rdf/rdf "first")}
;;           ::rdf/object {::rdf/literal "3"}}
;;          {::rdf/predicate {::rdf/iri (rdf/rdf "rest")}
;;           ::rdf/object {::rdf/iri (rdf/rdf "nil")}}]}}]}}])

;; (def ex-list
;;   (->> [1 2 3]
;;        (map str)
;;        (map (fn [x] {::rdf/literal x}))))

;; (def ex-long-list
;;   (->> (range 1 10000)
;;        (map str)
;;        (map (fn [x] {::rdf/literal x}))))

;; (deftest test-branch->list
;;   (is (=  ex-list
;;           (->> ex-list-branch
;;                omn/branch->list))))

;; (deftest test-list->branch
;;   (is (= ex-list
;;          (->> ex-list
;;               omn/list->branch
;;               ::rdf/pairs
;;               omn/branch->list)))
;;   (is (= ex-list
;;          (->> ex-list-branch
;;               ;; (mapcat rdf/unbranch-quad)
;;               (map ::rdf/object)
;;               (filter ::rdf/literal))))
;;   (is (= ex-long-list
;;          (->> ex-long-list
;;               omn/list->branch
;;               ::rdf/pairs
;;               omn/branch->list)))
;;   (is (= ex-long-list
;;          (->> ex-long-list
;;               omn/list->branch
;;               (assoc {} ::rdf/object)
;;               ;; rdf/unbranch-quad
;;               (map ::rdf/object)
;;               (filter ::rdf/literal)))))

;; (defn test-round-trip
;;   [content]
;;   (->> content
;;        omn/parse-class-expression
;;        (omn/convert-class-expression env-1)
;;        (omn/render-class-expression env-1)
;;        omn/write-class-expression
;;        (= content)
;;        is))

;; (deftest test-round-trips
;;   (test-round-trip "foo")
;;   (test-round-trip "'foo bar'")
;;   (test-round-trip "not foo")
;;   (test-round-trip "foo or bar")
;;   (test-round-trip "foo or foo or foo")
;;   (test-round-trip "foo and bar")
;;   (test-round-trip "'has part' some foo")
;;   (test-round-trip "'has part' only foo")
;;   (test-round-trip "'has part' some (foo or bar)")
;;   ; TODO: handle brackets and indentation better
;;   (test-round-trip "'is about' some ('material entity' and 'has role' some 'evaluant role')")
;;   (test-round-trip "has_specified_output some ('information content entity' and 'is about' some ('material entity' and 'has role' some 'evaluant role'))"))
