(ns org.knotation.json-ld-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.data.json :as json]
            [org.knotation.util :as util]
            [org.knotation.environment :as en]
            [org.knotation.format :as fm]
            [org.knotation.kn :as kn]
            [org.knotation.json-ld :as json-ld]))

(def example-kn "@prefix ex: <http://ex.com/>

: ex:foo
type: Bar
label: Foo
French; @fr: Fou
obsolete; boolean: true")

(def example-json-ld
  {"@context"
   {"ex" "http://ex.com/"
    "type" "ex:type"
    "label" "ex:label"
    "obsolete" "ex:obsolete"
    "boolean" "ex:boolean"
    "Bar" "ex:bar"
    "French" "ex:french"}
   "@id" "ex:foo"
   "iri" "http://ex.com/foo"
   "curie" "ex:foo"
   "type" {"@id" "ex:bar" "iri" "http://ex.com/bar" "label" "Bar"}
   "label" {"@value" "Foo"}
   "French" {"@value" "Fou" "@language" "fr"}
   "obsolete" {"@value" "true" "@type" "boolean"}})

(def env
  (-> en/default-env
      (en/add-prefix "ex" "http://ex.com/")
      (en/add-label "label" "http://ex.com/label")
      (en/add-label "type" "http://ex.com/type")
      (en/set-datatype "http://ex.com/type" "https://knotation.org/kn/link")
      (en/add-label "Bar" "http://ex.com/bar")
      (en/add-label "French" "http://ex.com/french")
      (en/add-label "obsolete" "http://ex.com/obsolete")
      (en/add-label "boolean" "http://ex.com/boolean")))

(deftest test-json-ld
  (is (= (->> example-kn
              clojure.string/split-lines
              (fm/read-lines :kn env)
              (json-ld/render-stanza-edn env "http://ex.com/foo"))
         example-json-ld)))
