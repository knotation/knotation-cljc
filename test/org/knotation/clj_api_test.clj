(ns org.knotation.clj-api-test
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [org.knotation.clj-api :as api]))

(deftest test-nt->edn
  (->> "<s> <p> <o> .
<s> <p> \"o\" .
<s> <p> \"o\"@l .
<s> <p> \"o\"^^<d> ."
       (api/read-string :nt nil)
       (= [{:si "s" :pi "p" :oi "o"}
           {:si "s" :pi "p" :ol "o" :di "http://www.w3.org/2001/XMLSchema#string"}
           {:si "s" :pi "p" :ol "o" :ln "l"}
           {:si "s" :pi "p" :ol "o" :di "d"}])
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
  ex:p \"o\"^^ex:d ."
       (api/read-string :ttl nil)
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
            :di "http://example.com/d"}])
       is))
