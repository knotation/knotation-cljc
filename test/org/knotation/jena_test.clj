(ns org.knotation.jena-test
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [orchestra.spec.test :as stest]

            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]
            [org.knotation.jena :as jena]
            [org.knotation.jena-spec]))

(stest/instrument)

(deftest test-nt->edn
  (->> "<s> <p> <o> .
<s> <p> \"o\" .
<s> <p> \"o\"@l .
<s> <p> \"o\"^^<d> .
_:s <p> _:o ."
       (jena/read-string :nt st/default-state)
       st/sequential-blank-nodes
       (= [{::st/event ::st/stanza-start
            ::rdf/stanza "s"}
           {::st/event ::st/subject-start
            ::rdf/stanza "s"
            ::rdf/subject "s"}
           {::st/event ::st/statement
            ::rdf/stanza "s"
            ::rdf/subject "s"
            ::rdf/quad #::rdf{:zn "s" :si "s" :pi "p" :oi "o"}}
           {::st/event ::st/statement
            ::rdf/stanza "s"
            ::rdf/subject "s"
            ::rdf/quad #::rdf{:zn "s" :si "s" :pi "p" :ol "o" :di "http://www.w3.org/2001/XMLSchema#string"}}
           {::st/event ::st/statement
            ::rdf/stanza "s"
            ::rdf/subject "s"
            ::rdf/quad #::rdf{:zn "s" :si "s" :pi "p" :ol "o" :lt "l"}}
           {::st/event ::st/statement
            ::rdf/stanza "s"
            ::rdf/subject "s"
            ::rdf/quad #::rdf{:zn "s" :si "s" :pi "p" :ol "o" :di "d"}}
           {::st/event ::st/subject-end
            ::rdf/stanza "s"
            ::rdf/subject "s"}
           {::st/event ::st/stanza-end
            ::rdf/stanza "s"}
           {::st/event ::st/blank
            ::rdf/stanza "s"}
           {::st/event ::st/stanza-start
            ::rdf/stanza "_:b0"}
           {::st/event ::st/subject-start
            ::rdf/stanza "_:b0"
            ::rdf/subject "_:b0"}
           {::st/event ::st/statement
            ::rdf/stanza "_:b0"
            ::rdf/subject "_:b0"
            ::rdf/quad #::rdf{:zn "_:b0" :sb "_:b0" :pi "p" :ob "_:b1"}}
           {::st/event ::st/subject-end
            ::rdf/stanza "_:b0"
            ::rdf/subject "_:b0"}
           {::st/event ::st/stanza-end
            ::rdf/stanza "_:b0"}])
       is))

(def test-ttl-string
  "@prefix ex: <http://example.com/> .
@base <http://example.com/> .

ex:s
  ex:p <http://example.com/o> ;
  ex:p <o> ;
  ex:p ex:o ;
  ex:p \"o\" ;
  ex:p \"o\"@l ;
  ex:p \"o\"^^ex:d ;
  ex:p _:o .")

(def test-ttl-edn
  [{::st/event ::st/prefix ::en/prefix "ex" ::en/iri "http://example.com/"}
   {::st/event ::st/base
    ::en/base "http://example.com/"}
   {::st/event ::st/blank}
   {::st/event ::st/stanza-start
    ::rdf/stanza "http://example.com/s"}
   {::st/event ::st/subject-start
    ::rdf/stanza "http://example.com/s"
    ::rdf/subject "http://example.com/s"}
   {::st/event ::st/statement
    ::rdf/stanza "http://example.com/s"
    ::rdf/subject "http://example.com/s"
    ::rdf/quad
    #::rdf{:zn "http://example.com/s"
           :si "http://example.com/s"
           :pi "http://example.com/p"
           :oi "http://example.com/o"}}
   {::st/event ::st/statement
    ::rdf/stanza "http://example.com/s"
    ::rdf/subject "http://example.com/s"
    ::rdf/quad
    #::rdf{:zn "http://example.com/s"
           :si "http://example.com/s"
           :pi "http://example.com/p"
           :oi "http://example.com/o"}}
   {::st/event ::st/statement
    ::rdf/stanza "http://example.com/s"
    ::rdf/subject "http://example.com/s"
    ::rdf/quad
    #::rdf{:zn "http://example.com/s"
           :si "http://example.com/s"
           :pi "http://example.com/p"
           :oi "http://example.com/o"}}
   {::st/event ::st/statement
    ::rdf/stanza "http://example.com/s"
    ::rdf/subject "http://example.com/s"
    ::rdf/quad
    #::rdf{:zn "http://example.com/s"
           :si "http://example.com/s"
           :pi "http://example.com/p"
           :ol "o"
           :di "http://www.w3.org/2001/XMLSchema#string"}}
   {::st/event ::st/statement
    ::rdf/stanza "http://example.com/s"
    ::rdf/subject "http://example.com/s"
    ::rdf/quad
    #::rdf{:zn "http://example.com/s"
           :si "http://example.com/s"
           :pi "http://example.com/p"
           :ol "o"
           :lt "l"}}
   {::st/event ::st/statement
    ::rdf/stanza "http://example.com/s"
    ::rdf/subject "http://example.com/s"
    ::rdf/quad
    #::rdf{:zn "http://example.com/s"
           :si "http://example.com/s"
           :pi "http://example.com/p"
           :ol "o"
           :di "http://example.com/d"}}
   {::st/event ::st/statement
    ::rdf/stanza "http://example.com/s"
    ::rdf/subject "http://example.com/s"
    ::rdf/quad
    #::rdf{:zn "http://example.com/s"
           :si "http://example.com/s"
           :pi "http://example.com/p"
           :ob "_:b0"}}
   {::st/event ::st/subject-end
    ::rdf/stanza "http://example.com/s"
    ::rdf/subject "http://example.com/s"}
   {::st/event ::st/stanza-end
    ::rdf/stanza "http://example.com/s"}])

(deftest test-ttl->edn
  (->> test-ttl-string
       (jena/read-string :ttl st/default-state)
       st/sequential-blank-nodes
       (= test-ttl-edn)
       is))

(deftest test-rdfxml->edn
  (->> "<?xml version=\"1.0\"?>
<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
     xmlns:ex=\"http://example.com/\">
    <ex:foo rdf:about=\"http://example.com/s\">
        <ex:p rdf:resource=\"http://example.com/o\"/>
        <ex:p rdf:datatype=\"http://www.w3.org/2001/XMLSchema#string\">o</ex:p>
        <ex:p xml:lang=\"l\">o</ex:p>
        <ex:p rdf:datatype=\"http://example.com/d\">o</ex:p>
    </ex:foo>
</rdf:RDF>"
       (jena/read-string :rdfxml st/default-state)
       (= [{::st/event ::st/prefix
            ::en/prefix "rdf"
            ::en/iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#"}
           {::st/event ::st/prefix
            ::en/prefix "ex"
            ::en/iri "http://example.com/"}
           {::st/event ::st/blank}
           {::st/event ::st/stanza-start
            ::rdf/stanza "http://example.com/s"}
           {::st/event ::st/subject-start
            ::rdf/stanza "http://example.com/s"
            ::rdf/subject "http://example.com/s"}
           {::st/event ::st/statement
            ::rdf/stanza "http://example.com/s"
            ::rdf/subject "http://example.com/s"
            ::rdf/quad
            #::rdf{:zn "http://example.com/s"
                   :si "http://example.com/s"
                   :pi "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
                   :oi "http://example.com/foo"}}
           {::st/event ::st/statement
            ::rdf/stanza "http://example.com/s"
            ::rdf/subject "http://example.com/s"
            ::rdf/quad
            #::rdf{:zn "http://example.com/s"
                   :si "http://example.com/s"
                   :pi "http://example.com/p"
                   :oi "http://example.com/o"}}
           {::st/event ::st/statement
            ::rdf/stanza "http://example.com/s"
            ::rdf/subject "http://example.com/s"
            ::rdf/quad
            #::rdf{:zn "http://example.com/s"
                   :si "http://example.com/s"
                   :pi "http://example.com/p"
                   :ol "o"
                   :di "http://www.w3.org/2001/XMLSchema#string"}}
           {::st/event ::st/statement
            ::rdf/stanza "http://example.com/s"
            ::rdf/subject "http://example.com/s"
            ::rdf/quad
            #::rdf{:zn "http://example.com/s"
                   :si "http://example.com/s"
                   :pi "http://example.com/p"
                   :ol "o"
                   :lt "l"}}
           {::st/event ::st/statement
            ::rdf/stanza "http://example.com/s"
            ::rdf/subject "http://example.com/s"
            ::rdf/quad
            #::rdf{:zn "http://example.com/s"
                   :si "http://example.com/s"
                   :pi "http://example.com/p"
                   :ol "o"
                   :di "http://example.com/d"}}
           {::st/event ::st/subject-end
            ::rdf/stanza "http://example.com/s"
            ::rdf/subject "http://example.com/s"}
           {::st/event ::st/stanza-end
            ::rdf/stanza "http://example.com/s"}])
       is))
