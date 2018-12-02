(ns org.knotation.cljs-api-test
  (:require [cljs.test :refer-macros [async deftest is testing]]
            [org.knotation.examples :as ex]
            [org.knotation.state :as st]
            [org.knotation.cljs-api :as api]))

(defn kn->kn
  [s]
  (->> s
      (api/read-input :kn nil)
      (api/render-output :kn nil)))

(defn kn->ttl
  [s]
  (->> s
      (api/read-input :kn nil)
      st/sequential-blank-nodes
      (api/render-output :ttl :nil)))

(defn test-kn-roundtrip
  [s]
  (let [out (kn->kn s)]
    (.log js/console out)
    (is (= s out))))

(defn test-kn-ttl-roundtrip
  [k t]
  (let [out (kn->ttl k)]
    (.log js/console out)
    (is (= t out))))

(deftest test-kn
  ;;(test-kn-roundtrip ex/basic-datatypes-kn))
  (test-kn-roundtrip ex/basic-labels-kn))
    ;;(test-kn-roundtrip ex/anonymous-subjects-kn)
    ;;(test-kn-roundtrip ex/basic-lists-kn)
    ;;(test-kn-roundtrip ex/basic-annotations-kn)
    ;;(test-kn-roundtrip ex/nested-annotations-kn)))

(deftest test-kn-ttl
  (test-kn-ttl-roundtrip ex/basic-datatypes-kn     ex/basic-datatypes-ttl))
    ;;(test-kn-ttl-roundtrip ex/basic-labels-kn        ex/basic-labels-ttl)
    ;;(test-kn-ttl-roundtrip ex/anonymous-subjects-kn  ex/anonymous-subjects-ttl)
    ;;(test-kn-ttl-roundtrip ex/basic-lists-kn         ex/basic-lists-ttl)
    ;;(test-kn-ttl-roundtrip ex/basic-annotations-kn   ex/basic-annotations-ttl)
    ;;(test-kn-ttl-roundtrip ex/nested-annotations-kn  ex/nested-annotations-ttl)))
