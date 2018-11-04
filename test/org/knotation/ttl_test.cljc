(ns org.knotation.ttl-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as string]
            [orchestra.spec.test :as stest]

            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]
            [org.knotation.ttl :as ttl]
            [org.knotation.ttl-spec]
            [org.knotation.api :as api]))

(stest/instrument)

(deftest test-render-state
  (is (= (ttl/render-prefix
          {::st/event ::st/prefix :prefix "kn" :iri "https://knotation.org/kn/"})
         "@prefix kn: <https://knotation.org/kn/> .\n"))
  (is (= (ttl/render-base
          {::st/event ::st/base :base "http://example.com/"})
         "@base <http://example.com/> .\n"))
  (is (= (ttl/render-subject-start
          {::st/event ::st/subject-start :subject "http://example.com/s" :terminal "\n"})
         "<http://example.com/s>\n"))
  (is (= (ttl/render-subject-end
          {::st/event ::st/subject-end :subject "http://example.com/s"})
         nil))
  (is (= (ttl/render-statement
          {::st/event ::st/statement
           :terminal " ;\n"
           ::rdf/quad
           #::rdf{:zn "http://example.com/s"
                  :si "http://example.com/s"
                  :pi "http://example.com/p"
                  :oi "http://example.com/o"}})
         "  <http://example.com/p> <http://example.com/o> ;\n"))
  (is (= (ttl/render-statement
          {::st/event ::st/statement
           :terminal " ;\n"
           ::rdf/quad
           #::rdf{:zn "http://example.com/s"
                  :si "http://example.com/s"
                  :pi "http://example.com/p"
                  :ob "_:o"}})
         "  <http://example.com/p> _:o ;\n"))
  (is (= (ttl/render-statement
          {::st/event ::st/statement
           :terminal " ;\n"
           ::rdf/quad
           #::rdf{:zn "http://example.com/s"
                  :si "http://example.com/s"
                  :pi "http://example.com/p"
                  :ol "o"}})
         "  <http://example.com/p> \"o\" ;\n"))
  (is (= (ttl/render-statement
          {::st/event ::st/statement
           :terminal " ;\n"
           ::rdf/quad
           #::rdf{:zn "http://example.com/s"
                  :si "http://example.com/s"
                  :pi "http://example.com/p"
                  :ol "o"
                  :di "http://example.com/d"}})
         "  <http://example.com/p> \"o\"^^<http://example.com/d> ;\n"))
  (is (= (ttl/render-statement
          {::st/event ::st/statement
           :terminal " ;\n"
           ::rdf/quad
           #::rdf{:zn "http://example.com/s"
                  :si "http://example.com/s"
                  :pi "http://example.com/p"
                  :ol "o"
                  :lt "en"}})
         "  <http://example.com/p> \"o\"@en ;\n")))

(def test-sorting-before
  [{::st/event ::st/statement ::rdf/quad #::rdf{:zn "s" :sb "_:b0" :pi (rdf/rdf "first") :ol "1"}}
   {::st/event ::st/statement ::rdf/quad #::rdf{:zn "s" :sb "_:b0" :pi (rdf/rdf "rest") :oi (rdf/rdf "nil")}}
   {::st/event ::st/statement ::rdf/quad #::rdf{:zn "s" :si "s" :pi "p" :ol "before"}}
   {::st/event ::st/statement ::rdf/quad #::rdf{:zn "s" :si "s" :pi "p" :ob "_:b0"}}
   {::st/event ::st/statement ::rdf/quad #::rdf{:zn "s" :si "s" :pi "p" :ol "after"}}])

(def test-sorting-after
  [{::st/event ::st/subject-start :subject "s"}
   {::st/event ::st/statement ::rdf/quad #::rdf{:zn "s" :si "s" :pi "p" :ol "before"}}
   {::st/event ::st/statement ::rdf/quad #::rdf{:zn "s" :si "s" :pi "p" :ob "_:b0"}}
   {::st/event ::st/subject-start :subject "_:b0"}
   {::st/event ::st/statement ::rdf/quad #::rdf{:zn "s" :sb "_:b0" :pi (rdf/rdf "first") :ol "1"}}
   {::st/event ::st/statement ::rdf/quad #::rdf{:zn "s" :sb "_:b0" :pi (rdf/rdf "rest") :oi (rdf/rdf "nil")}}
   {::st/event ::st/subject-end :subject "_:b0"}
   {::st/event ::st/statement ::rdf/quad #::rdf{:zn "s" :si "s" :pi "p" :ol "after"}}
   {::st/event ::st/subject-end :subject "s"}])

(deftest test-sorting
  (->> test-sorting-before
       ttl/sort-stanza
       (map #(select-keys % [::st/event ::rdf/quad :subject]))
       (= test-sorting-after)
       is))

(def test-input-edn
  [{::st/event ::st/prefix :prefix "kn" :iri "https://knotation.org/kn/"}
   {::st/event ::st/prefix :prefix "ex" :iri "http://example.com/"}
   {::st/event ::st/statement
    ::rdf/quad #::rdf{:zn "http://example.com/s"
                      :si "http://example.com/s"
                      :pi "http://example.com/p"
                      :oi "http://example.com/o"}}
   {::st/event ::st/statement
    ::rdf/quad #::rdf{:zn "http://example.com/s"
                      :si "http://example.com/s"
                      :pi "http://example.com/p"
                      :ol "o"}}])

(def test-output-env
  (-> {}
      (en/add-prefix "kn" "https://knotation.org/kn/")
      (en/add-prefix "ex" "http://example.com/")))

(def test-output-edn
  [{::st/event ::st/prefix :prefix "kn" :iri "https://knotation.org/kn/"
    ::en/env {}
    :line-number 2
    :column-number 1
    ::st/output
    #::st{:format :ttl
          :content "@prefix kn: <https://knotation.org/kn/> .\n"
          :line-number 1
          :column-number 1}}
   {::st/event ::st/prefix :prefix "ex" :iri "http://example.com/"
    ::en/env (en/add-prefix {} "kn" "https://knotation.org/kn/")
    :line-number 3
    :column-number 1
    ::st/output
    #::st{:format :ttl
          :content "@prefix ex: <http://example.com/> .\n"
          :line-number 2
          :column-number 1}}
   {::st/event ::st/blank
    ::en/env test-output-env
    :line-number 4
    :column-number 1
    ::st/output
    #::st{:format :ttl
          :content "\n"
          :line-number 3
          :column-number 1}}
   {::st/event ::st/subject-start :subject "http://example.com/s"
    ::en/env test-output-env
    :depth 0
    :line-number 5
    :column-number 1
    :terminal "\n"
    ::st/output
    #::st{:format :ttl
          :content "ex:s\n"
          :line-number 4
          :column-number 1}}
   {::st/event ::st/statement
    ::rdf/quad #::rdf{:zn "http://example.com/s"
                      :si "http://example.com/s"
                      :pi "http://example.com/p"
                      :oi "http://example.com/o"}
    ::en/env test-output-env
    :depth 1
    :line-number 6
    :column-number 1
    :terminal " ;\n"
    ::st/output
    #::st{:format :ttl
          :content "  ex:p ex:o ;\n"
          :line-number 5
          :column-number 1}}
   {::st/event ::st/statement
    ::rdf/quad #::rdf{:zn "http://example.com/s"
                      :si "http://example.com/s"
                      :pi "http://example.com/p"
                      :ol "o"}
    ::en/env test-output-env
    :line-number 7
    :column-number 1
    :depth 1
    :last true
    :terminal " .\n"
    ::st/output
    #::st{:format :ttl
          :content "  ex:p \"o\" .\n"
          :line-number 6
          :column-number 1}}
   {::st/event ::st/subject-end :subject "http://example.com/s"
    ::en/env test-output-env
    :depth 0
    :line-number 7
    :column-number 1
    :list-item false
    :terminal "\n"}])

(deftest test-edn->edn
  (->> test-input-edn
       (ttl/render-states {})
       (= test-output-edn)
       is))

(def test-input-list
  [{::st/event ::st/stanza-start :subject "http://example.com/s"}
   {::st/event ::st/statement
    ::rdf/quad #::rdf{:sb "_:b0"
                      :pi (rdf/rdf "first")
                      :oi "http://example.com/a"}}
   {::st/event ::st/statement
    ::rdf/quad #::rdf{:sb "_:b0"
                      :pi (rdf/rdf "rest")
                      :ob "_:b1"}}
   {::st/event ::st/statement
    ::rdf/quad #::rdf{:sb "_:b1"
                      :pi (rdf/rdf "first")
                      :oi "http://example.com/b"}}
   {::st/event ::st/statement
    ::rdf/quad #::rdf{:sb "_:b1"
                      :pi (rdf/rdf "rest")
                      :ob "_:b2"}}
   {::st/event ::st/statement
    ::rdf/quad #::rdf{:sb "_:b2"
                      :pi (rdf/rdf "first")
                      :oi "http://example.com/c"}}
   {::st/event ::st/statement
    ::rdf/quad #::rdf{:sb "_:b2"
                      :pi (rdf/rdf "rest")
                      :oi (rdf/rdf "nil")}}
   {::st/event ::st/statement
    ::rdf/quad #::rdf{:si "http://example.com/s"
                      :pi "http://example.com/p"
                      :ob "_:b0"}}
   {::st/event ::st/stanza-end :subject "http://example.com/s"}])

(def test-output-list
  [{::st/event ::st/stanza-start :subject "http://example.com/s"
    ::en/env {}
    :line-number 2
    :column-number 1
    ::st/output
    #::st{:format :ttl
          :content "<http://example.com/s>\n"
          :line-number 1
          :column-number 1}}
   {::st/event ::st/statement
    ::rdf/quad #::rdf{:si "http://example.com/s"
                      :pi "http://example.com/p"
                      :ob "_:b0"}
    ::en/env {}
    :line-number 3
    :column-number 1
    :depth 1
    ::st/output
    #::st{:format :ttl
          :content "  <http://example.com/p> (\n"
          :line-number 2
          :column-number 1}}
   {::st/event ::st/statement
    ::rdf/quad #::rdf{:sb "_:b0"
                      :pi (rdf/rdf "first")
                      :oi "http://example.com/a"}}
   {::st/event ::st/statement
    ::rdf/quad #::rdf{:sb "_:b0"
                      :pi (rdf/rdf "rest")
                      :ob "_:b1"}}
   {::st/event ::st/statement
    ::rdf/quad #::rdf{:sb "_:b1"
                      :pi (rdf/rdf "first")
                      :oi "http://example.com/b"}}
   {::st/event ::st/statement
    ::rdf/quad #::rdf{:sb "_:b1"
                      :pi (rdf/rdf "rest")
                      :ob "_:b2"}}
   {::st/event ::st/statement
    ::rdf/quad #::rdf{:sb "_:b2"
                      :pi (rdf/rdf "first")
                      :oi "http://example.com/c"}}
   {::st/event ::st/statement
    ::rdf/quad #::rdf{:sb "_:b2"
                      :pi (rdf/rdf "rest")
                      :oi (rdf/rdf "nil")}}
   {::st/event ::st/stanza-end :subject "http://example.com/s"}])

(deftest test-list
  #_(->> test-input-list
         (take 2)
         (ttl/render-stanza [])
         (= (take 2 test-output-list))
         is))

(comment
  (->> test-sorting
       ttl/sort-stanza
       ;(map #(dissoc % ::st/event))
       (map ::rdf/quad)
       (map println))

  (->> test-input-list
       ttl/sort-stanza
       (map println))

  (->> test-input-list
       ;(take 2)
       (ttl/render-stanza [])
       (map ::st/output)
       (map ::st/content)
       string/join
       println)

  (->> test-output-list
       (take 2)
       (map ::st/output)
       (map ::st/content)
       string/join
       println))
