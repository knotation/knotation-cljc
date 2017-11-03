(ns org.knotation.nq-test
  (:require [clojure.test :refer [deftest is testing]]
            [org.knotation.state :as st]
            [org.knotation.nq :as nq]))

(deftest test-parse-quad
  (is (= (nq/parse-quad "<s> <p> <o> <g> .")
         {:graph {:iri "g"}
          :subject {:iri "s"}
          :predicate {:iri "p"}
          :object {:iri "o"}}))
  (is (= (nq/parse-quad "<s> <p> <o> .")
         {:graph nil
          :subject {:iri "s"}
          :predicate {:iri "p"}
          :object {:iri "o"}}))
  (is (= (nq/parse-quad "_:b1 <p> _:b2 .")
         {:graph nil
          :subject {:bnode "_:b1"}
          :predicate {:iri "p"}
          :object {:bnode "_:b2"}}))
  (is (= (nq/parse-quad "<s> <p> \"o\" .")
         {:graph nil
          :subject {:iri "s"}
          :predicate {:iri "p"}
          :object {:lexical "o"}}))
  (is (= (nq/parse-quad "<s> <p> \"o\"@en .")
         {:graph nil
          :subject {:iri "s"}
          :predicate {:iri "p"}
          :object {:lexical "o" :language "en"}}))
  (is (= (nq/parse-quad "<s> <p> \"o\"^^<d> .")
         {:graph nil
          :subject {:iri "s"}
          :predicate {:iri "p"}
          :object {:lexical "o" :datatype "d"}})))
