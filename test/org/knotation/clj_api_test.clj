(ns org.knotation.clj-api-test
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [org.knotation.rdf :as rdf]
            [org.knotation.clj-api :as api]))

(deftest test-nt->edn
  (->> "<s> <p> <o> .
<s> <p> \"o\" .
<s> <p> \"o\"@l .
<s> <p> \"o\"^^<d> .
_:s <p> _:o ."
       (api/read-string :nt nil)
       rdf/sequential-blank-nodes
       (= [{:si "s" :pi "p" :oi "o"}
           {:si "s" :pi "p" :ol "o" :di "http://www.w3.org/2001/XMLSchema#string"}
           {:si "s" :pi "p" :ol "o" :ln "l"}
           {:si "s" :pi "p" :ol "o" :di "d"}
           {:sb "_:b0" :pi "p" :ob "_:b1"}])
       is))

(deftest test-ttl->edn
  (->> "@prefix ex: <http://example.com/> .
@base <http://example.com/> .

ex:s
  ex:p <http://example.com/o> ;
  ex:p <o> ;
  ex:p ex:o ;
  ex:p \"o\" ;
  ex:p \"o\"@l ;
  ex:p \"o\"^^ex:d ;
  ex:p _:o ."
       (api/read-string :ttl nil)
       rdf/sequential-blank-nodes
       (= [{:prefix "ex" :iri "http://example.com/"}
           {:base "http://example.com/"}
           {:si "http://example.com/s"
            :pi "http://example.com/p"
            :oi "http://example.com/o"}
           {:si "http://example.com/s"
            :pi "http://example.com/p"
            :oi "http://example.com/o"}
           {:si "http://example.com/s"
            :pi "http://example.com/p"
            :oi "http://example.com/o"}
           {:si "http://example.com/s"
            :pi "http://example.com/p"
            :ol "o"
            :di "http://www.w3.org/2001/XMLSchema#string"}
           {:si "http://example.com/s"
            :pi "http://example.com/p"
            :ol "o"
            :ln "l"}
           {:si "http://example.com/s"
            :pi "http://example.com/p"
            :ol "o"
            :di "http://example.com/d"}
           {:si "http://example.com/s"
            :pi "http://example.com/p"
            :ob "_:b0"}])
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
       (api/read-string :rdfxml nil)
       (= [{:prefix "rdf" :iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#"}
           {:prefix "ex" :iri "http://example.com/"}
           {:si "http://example.com/s"
            :pi "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
            :oi "http://example.com/foo"}
           {:si "http://example.com/s"
            :pi "http://example.com/p"
            :oi "http://example.com/o"}
           {:si "http://example.com/s"
            :pi "http://example.com/p"
            :ol "o"
            :di "http://www.w3.org/2001/XMLSchema#string"}
           {:si "http://example.com/s"
            :pi "http://example.com/p"
            :ol "o"
            :ln "l"}
           {:si "http://example.com/s"
            :pi "http://example.com/p"
            :ol "o"
            :di "http://example.com/d"}])
       is))
