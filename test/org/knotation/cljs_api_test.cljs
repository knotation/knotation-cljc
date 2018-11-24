(ns org.knotation.cljs-api-test
  (:require [cljs.test :refer-macros [async deftest is testing]]
            [org.knotation.examples :as ex]
            [org.knotation.state :as st]
            [org.knotation.cljs-api :as api]))

(defn kn->kn
  [s]
  (async done
    (->> s
         (api/read-input :kn nil)
         (api/render-output :kn nil))
    (done)))

(defn kn->ttl
  [s]
  (async done
    (->> s
         (api/read-input :kn nil)
         st/sequential-blank-nodes
         (api/render-output :ttl :nil))
  (done)))

(defn test-kn-roundtrip
  [s]
  (async done
    (is (= s (kn->kn s)))
    (done)))

(defn test-kn-ttl-roundtrip
  [k t]
  (async done
    (is (= t (kn->ttl k)))
    (done)))

(deftest test-kn
  (async done
    (test-kn-roundtrip ex/basic-datatypes-kn)
    (test-kn-roundtrip ex/basic-labels-kn)
    (test-kn-roundtrip ex/anonymous-subjects-kn)
    (test-kn-roundtrip ex/basic-lists-kn)
    (test-kn-roundtrip ex/basic-annotations-kn)
    (test-kn-roundtrip ex/nested-annotations-kn)
    (done)))

(deftest test-kn-ttl
  (async done
    (test-kn-ttl-roundtrip ex/basic-datatypes-kn     ex/basic-datatypes-ttl)
    (test-kn-ttl-roundtrip ex/basic-labels-kn        ex/basic-labels-ttl)
    (test-kn-ttl-roundtrip ex/anonymous-subjects-kn  ex/anonymous-subjects-ttl)
    (test-kn-ttl-roundtrip ex/basic-lists-kn         ex/basic-lists-ttl)
    (test-kn-ttl-roundtrip ex/basic-annotations-kn   ex/basic-annotations-ttl)
    (test-kn-ttl-roundtrip ex/nested-annotations-kn  ex/nested-annotations-ttl)
    (done)))
