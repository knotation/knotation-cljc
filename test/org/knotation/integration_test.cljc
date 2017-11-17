(ns org.knotation.integration-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.string :as string]
            [org.knotation.api :as api]))

(stest/instrument)

(def ex-1-kn
  "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
@prefix ex: <https://example.com/>

: ex:123
rdfs:label: owl 123
ex:translation; @fr: hibou 123
ex:has-wingspan; ex:inches: 22")

(def ex-1-ttl
  "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix ex: <https://example.com/> .

ex:123
  rdfs:label \"owl 123\" ;
  ex:translation \"hibou 123\"@fr ;
  ex:has-wingspan \"22\"^^ex:inches .")

(deftest ex-1
  (is (= ex-1-ttl
         (->> [(api/input :kn ex-1-kn)
               (api/output :ttl)]
              api/run-operations
              api/content))))

(def ex-2-kn
  "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
@prefix ex: <https://example.com/>

: rdfs:label
rdfs:label: label

: ex:translation
label: translation

: ex:inches
label: inches

: ex:has-wingspan
label: has wingspan

: ex:123
label: owl 123
translation; @fr: hibou 123
has wingspan; inches: 22")

(def ex-2-ttl
  "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix ex: <https://example.com/> .

rdfs:label
  rdfs:label \"label\" .

ex:translation
  rdfs:label \"translation\" .

ex:inches
  rdfs:label \"inches\" .

ex:has-wingspan
  rdfs:label \"has wingspan\" .

ex:123
  rdfs:label \"owl 123\" ;
  ex:translation \"hibou 123\"@fr ;
  ex:has-wingspan \"22\"^^ex:inches .")

(deftest ex-2
  (is (= ex-2-ttl
         (->> [(api/input :kn ex-2-kn)
               (api/output :ttl)]
              api/run-operations
              api/content))))

(def ex-3-env
  "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
@prefix ex: <https://example.com/>

: rdfs:label
rdfs:label: label

: ex:translation
label: translation

: ex:inches
label: inches

: ex:has-wingspan
label: has wingspan")

(def ex-3-kn
  ": ex:123
label: owl 123
translation; @fr: hibou 123
has wingspan; inches: 22")

(def ex-3-ttl
  "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix ex: <https://example.com/> .

ex:123
  rdfs:label \"owl 123\" ;
  ex:translation \"hibou 123\"@fr ;
  ex:has-wingspan \"22\"^^ex:inches .")

(deftest ex-3
  (is (= ex-3-ttl
         (->> [(api/env :kn ex-3-env)
               api/prefixes
               api/space
               (api/input :kn ex-3-kn)
               (api/output :ttl)]
              api/run-operations
              api/content))))

(def ex-4-env
  "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
@prefix kn: <https://knotation.org/>
@prefix ex: <https://example.com/>

: rdfs:label
rdfs:label: label

: kn:datatype/link
label: link

: kn:predicate/default-language
label: default language

: kn:predicate/default-datatype
label: default datatype
default datatype; link: link

: ex:translation
label: translation
default language: fr

: ex:inches
label: inches

: ex:has-wingspan
label: has wingspan
default datatype: inches")

(def ex-4-kn
  ": ex:123
label: owl 123
translation: hibou 123
has wingspan: 22")

(def ex-4-ttl
  "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix kn: <https://knotation.org/> .
@prefix ex: <https://example.com/> .

ex:123
  rdfs:label \"owl 123\" ;
  ex:translation \"hibou 123\"@fr ;
  ex:has-wingspan \"22\"^^ex:inches .")

(deftest ex-4
  (is (= ex-4-ttl
         (->> [(api/env :kn ex-4-env)
               api/prefixes
               api/space
               (api/input :kn ex-4-kn)
               (api/output :ttl)]
              api/run-operations
              api/content))))

(def ex-5-env
  "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
@prefix kn: <https://knotation.org/>
@prefix ex: <https://example.com/>

: rdfs:label
rdfs:label: label

: kn:datatype/link
rdfs:label: link

: kn:predicate/default-datatype
label: default datatype
default datatype; link: link

: ex:has-label-link
label: has label link
default datatype: link

: ex:has-curie-link
label: has CURIE link
default datatype: link

: ex:has-httpurl-link
label: has HTTP URL link
default datatype: link

: ex:has-iri-link
label: has IRI link
default datatype: link")

(def ex-5-kn
  ": ex:labelled-object
rdfs:label: labelled object

: ex:some-subject
has label link: labelled object
has CURIE link: ex:curie-object
has HTTP URL link: http://example.com/url-object
has IRI link: <urn:ietf:rfc:2648>
ex:no-default; link: ex:curie-object")

(def ex-5-ttl
  "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix kn: <https://knotation.org/> .
@prefix ex: <https://example.com/> .

ex:labelled-object
  rdfs:label \"labelled object\" .

ex:some-subject
  ex:has-label-link ex:labelled-object ;
  ex:has-curie-link ex:curie-object ;
  ex:has-httpurl-link <http://example.com/url-object> ;
  ex:has-iri-link <urn:ietf:rfc:2648> ;
  ex:no-default ex:curie-object .")

(deftest ex-5
  (is (= ex-5-ttl
         (->> [(api/env :kn ex-5-env)
               api/prefixes
               api/space
               (api/input :kn ex-5-kn)
               (api/output :ttl)]
              api/run-operations
              api/content))))
