(ns org.knotation.jena-test
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [orchestra.spec.test :as stest]

            [org.knotation.rdf :as rdf]
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
       (jena/read-string "nt")
       st/sequential-blank-nodes
       (= [{::st/event ::st/statement
            ::rdf/quad #::rdf{:zn "s" :si "s" :pi "p" :oi "o"}}
           {::st/event ::st/statement
            ::rdf/quad #::rdf{:zn "s" :si "s" :pi "p" :ol "o" :di "http://www.w3.org/2001/XMLSchema#string"}}
           {::st/event ::st/statement
            ::rdf/quad #::rdf{:zn "s" :si "s" :pi "p" :ol "o" :lt "l"}}
           {::st/event ::st/statement
            ::rdf/quad #::rdf{:zn "s" :si "s" :pi "p" :ol "o" :di "d"}}
           {::st/event ::st/statement
            ::rdf/quad #::rdf{:zn "_:b0" :sb "_:b0" :pi "p" :ob "_:b1"}}])
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
  [{::st/event ::st/prefix :prefix "ex" :iri "http://example.com/"}
   {::st/event ::st/base :base "http://example.com/"}
   {::st/event ::st/statement
    ::rdf/quad
    #::rdf{:zn "http://example.com/s"
           :si "http://example.com/s"
           :pi "http://example.com/p"
           :oi "http://example.com/o"}}
   {::st/event ::st/statement
    ::rdf/quad
    #::rdf{:zn "http://example.com/s"
           :si "http://example.com/s"
           :pi "http://example.com/p"
           :oi "http://example.com/o"}}
   {::st/event ::st/statement
    ::rdf/quad
    #::rdf{:zn "http://example.com/s"
           :si "http://example.com/s"
           :pi "http://example.com/p"
           :oi "http://example.com/o"}}
   {::st/event ::st/statement
    ::rdf/quad
    #::rdf{:zn "http://example.com/s"
           :si "http://example.com/s"
           :pi "http://example.com/p"
           :ol "o"
           :di "http://www.w3.org/2001/XMLSchema#string"}}
   {::st/event ::st/statement
    ::rdf/quad
    #::rdf{:zn "http://example.com/s"
           :si "http://example.com/s"
           :pi "http://example.com/p"
           :ol "o"
           :lt "l"}}
   {::st/event ::st/statement
    ::rdf/quad
    #::rdf{:zn "http://example.com/s"
           :si "http://example.com/s"
           :pi "http://example.com/p"
           :ol "o"
           :di "http://example.com/d"}}
   {::st/event ::st/statement
    ::rdf/quad
    #::rdf{:zn "http://example.com/s"
           :si "http://example.com/s"
           :pi "http://example.com/p"
           :ob "_:b0"}}])

(deftest test-ttl->edn
  (->> test-ttl-string
       (jena/read-string "ttl")
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
       (jena/read-string "rdfxml")
       (= [{::st/event ::st/prefix :prefix "rdf" :iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#"}
           {::st/event ::st/prefix :prefix "ex" :iri "http://example.com/"}
           {::st/event ::st/statement
            ::rdf/quad
            #::rdf{:zn "http://example.com/s"
                   :si "http://example.com/s"
                   :pi "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
                   :oi "http://example.com/foo"}}
           {::st/event ::st/statement
            ::rdf/quad
            #::rdf{:zn "http://example.com/s"
                   :si "http://example.com/s"
                   :pi "http://example.com/p"
                   :oi "http://example.com/o"}}
           {::st/event ::st/statement
            ::rdf/quad
            #::rdf{:zn "http://example.com/s"
                   :si "http://example.com/s"
                   :pi "http://example.com/p"
                   :ol "o"
                   :di "http://www.w3.org/2001/XMLSchema#string"}}
           {::st/event ::st/statement
            ::rdf/quad
            #::rdf{:zn "http://example.com/s"
                   :si "http://example.com/s"
                   :pi "http://example.com/p"
                   :ol "o"
                   :lt "l"}}
           {::st/event ::st/statement
            ::rdf/quad
            #::rdf{:zn "http://example.com/s"
                   :si "http://example.com/s"
                   :pi "http://example.com/p"
                   :ol "o"
                   :di "http://example.com/d"}}])
       is))
