(ns org.knotation.rdf-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [org.knotation.rdf :as rdf]))

(stest/instrument)

(def ex-flat
  [{::rdf/subject {::rdf/iri "s"}
    ::rdf/predicate {::rdf/iri "p"}
    ::rdf/object {::rdf/bnode "_:1"}}
   {::rdf/subject {::rdf/bnode "_:1"}
    ::rdf/predicate {::rdf/iri "p"}
    ::rdf/object {::rdf/bnode "_:2"}}
   {::rdf/subject {::rdf/bnode "_:1"}
    ::rdf/predicate {::rdf/iri "p"}
    ::rdf/object {::rdf/literal "1"}}
   {::rdf/subject {::rdf/bnode "_:2"}
    ::rdf/predicate {::rdf/iri "p"}
    ::rdf/object {::rdf/literal "2"}}])

(def ex-branched
  {::rdf/subject {::rdf/iri "s"}
   ::rdf/predicate {::rdf/iri "p"}
   ::rdf/object
   {::rdf/bnode "_:A"
    ::rdf/pairs
    [{::rdf/predicate {::rdf/iri "p"}
      ::rdf/object
      {::rdf/bnode "_:B"
       ::rdf/pairs
       [{::rdf/predicate {::rdf/iri "p"}
         ::rdf/object {::rdf/literal "2"}}]}}
     {::rdf/predicate {::rdf/iri "p"}
      ::rdf/object {::rdf/literal "1"}}]}})

(def ex-blank
  {::rdf/subject {::rdf/bnode "_:BLANK"}
   ::rdf/predicate {::rdf/iri "p"}
   ::rdf/object {::rdf/literal "o"}})

(def ex-blank-2
  {::rdf/subject {::rdf/bnode "_:BLANK_2"}
   ::rdf/predicate {::rdf/iri "p"}
   ::rdf/object {::rdf/literal "o"}})

(def ex-named
  {::rdf/subject {::rdf/iri "NAMED"}
   ::rdf/predicate {::rdf/iri "p"}
   ::rdf/object {::rdf/literal "o"}})

; See https://stackoverflow.com/a/26076145
(defn permutations [s]
  (lazy-seq
   (if (seq (rest s))
     (apply
      concat
      (for [x s]
        (map #(cons x %) (permutations (remove #{x} s)))))
     [s])))

; See https://stackoverflow.com/a/8092016
(defn index-of [e coll] (first (keep-indexed #(if (= e %2) %1) coll)))

; Order does matter!
; In ex-flat the second quad should come before the third quad.
(defn good-permutation
  [quads]
  (< (index-of (get ex-flat 1) quads)
     (index-of (get ex-flat 2) quads)))

(def ex-sequential-branched
  (->> ex-branched
       vector
       rdf/sequential-blank-nodes))

(deftest test-branch
  (is (= ex-sequential-branched
         (rdf/branch-quads ex-flat))))
;  (doseq [permutation (->> ex-flat permutations (filter good-permutation))]
;    (is (= ex-sequential-branched
;           (rdf/branch-quads permutation))
;        "Handle scrambled order")
;  (is (= (concat [ex-named] ex-sequential-branched)
;         (rdf/branch-quads (concat [ex-named] ex-flat)))
;      "Order of named quads is preserved")
;  (is (= (concat ex-sequential-branched [ex-named])
;         (rdf/branch-quads (concat ex-flat [ex-named])))
;      "Order of named quads is preserved")
;  (doseq [permutation (->> (conj ex-flat ex-blank)
;                           permutations
;                           (filter good-permutation))]
;    (is (= (concat ex-sequential-branched [ex-blank])
;           (rdf/branch-quads permutation))
;        "Quads with blank subjects drop to bottom."))
;  (is (= (concat ex-sequential-branched [ex-blank ex-blank-2])
;         (rdf/branch-quads
;          (concat [ex-blank] (take 2 ex-flat) [ex-blank-2] (drop 2 ex-flat))))
;      "Quads with blank subjects drop to bottom in order."))

(deftest test-unbranch
  (is (= ex-flat
         (->> ex-branched
              rdf/unbranch-quad
              rdf/sequential-blank-nodes)))
  (is (= ex-flat
         (->> ex-branched
              vector
              rdf/sequential-blank-nodes
              first
              rdf/unbranch-quad))))
