(ns org.knotation.clj-api-test
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [orchestra.spec.test :as stest]

            [org.knotation.rdf :as rdf]
            [org.knotation.rdf-spec]
            [org.knotation.clj-api :as api]))

(stest/instrument)

(deftest test-nt->edn
  (->> "<s> <p> <o> .
<s> <p> \"o\" .
<s> <p> \"o\"@l .
<s> <p> \"o\"^^<d> .
_:s <p> _:o ."
       (api/read-string :nt nil)
       rdf/sequential-blank-nodes
       (= [#::rdf{:si "s" :pi "p" :oi "o"}
           #::rdf{:si "s" :pi "p" :ol "o" :di "http://www.w3.org/2001/XMLSchema#string"}
           #::rdf{:si "s" :pi "p" :ol "o" :lt "l"}
           #::rdf{:si "s" :pi "p" :ol "o" :di "d"}
           #::rdf{:sb "_:b0" :pi "p" :ob "_:b1"}])
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
           #::rdf{:si "http://example.com/s"
                  :pi "http://example.com/p"
                  :oi "http://example.com/o"}
           #::rdf{:si "http://example.com/s"
                  :pi "http://example.com/p"
                  :oi "http://example.com/o"}
           #::rdf{:si "http://example.com/s"
                  :pi "http://example.com/p"
                  :oi "http://example.com/o"}
           #::rdf{:si "http://example.com/s"
                  :pi "http://example.com/p"
                  :ol "o"
                  :di "http://www.w3.org/2001/XMLSchema#string"}
           #::rdf{:si "http://example.com/s"
                  :pi "http://example.com/p"
                  :ol "o"
                  :lt "l"}
           #::rdf{:si "http://example.com/s"
                  :pi "http://example.com/p"
                  :ol "o"
                  :di "http://example.com/d"}
           #::rdf{:si "http://example.com/s"
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
           #::rdf{:si "http://example.com/s"
                  :pi "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
                  :oi "http://example.com/foo"}
           #::rdf{:si "http://example.com/s"
                  :pi "http://example.com/p"
                  :oi "http://example.com/o"}
           #::rdf{:si "http://example.com/s"
                  :pi "http://example.com/p"
                  :ol "o"
                  :di "http://www.w3.org/2001/XMLSchema#string"}
           #::rdf{:si "http://example.com/s"
                  :pi "http://example.com/p"
                  :ol "o"
                  :lt "l"}
           #::rdf{:si "http://example.com/s"
                  :pi "http://example.com/p"
                  :ol "o"
                  :di "http://example.com/d"}])
       is))
