(ns org.knotation.clj-api-test
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [orchestra.spec.test :as stest]

            [org.knotation.examples :as ex]
            [org.knotation.state :as st]
            [org.knotation.clj-api :as api]
            [org.knotation.clj-api-spec]))

(stest/instrument)

(defn kn->kn
  [s]
  (->> s
       (api/read-string :kn nil)
       (api/render-string :kn nil)))

(defn ttl->ttl
  [s]
  (->> s
       (api/read-string :ttl nil)
       st/sequential-blank-nodes
       (api/render-string :ttl nil)))

(defn kn->ttl
  [s]
  (->> s
       (api/read-string :kn nil)
       st/sequential-blank-nodes
       (api/render-string :ttl nil)))

(defn ttl->kn
  [s]
  (->> s
       (api/read-string :ttl nil)
       st/sequential-blank-nodes
       (api/render-string :kn nil)))

(defn test-kn-roundtrip
  [s]
  (is (= s (kn->kn s))))

(defn test-ttl-roundtrip
  [s]
  (is (= s (ttl->ttl s))))

(defn test-kn-ttl-roundtrip
  [k t]
  (is (= t (kn->ttl k)))
  (is (= k (ttl->kn t))))

(deftest test-kn
  (test-kn-roundtrip ex/basic-datatypes-kn)
  (test-kn-roundtrip ex/basic-labels-kn)
  (test-kn-roundtrip ex/anonymous-subjects-kn)
  (test-kn-roundtrip ex/basic-annotations-kn)
  (test-kn-roundtrip ex/nested-annotations-kn))

(deftest test-ttl
  (test-ttl-roundtrip ex/basic-datatypes-ttl)
  (test-ttl-roundtrip ex/anonymous-subjects-ttl)
  (test-ttl-roundtrip ex/mixed-lists-ttl)
  (test-ttl-roundtrip ex/basic-annotations-ttl)
  (test-ttl-roundtrip ex/nested-annotations-ttl))

(deftest test-kn-ttl
  (test-kn-ttl-roundtrip ex/basic-datatypes-kn     ex/basic-datatypes-ttl)
  (test-kn-ttl-roundtrip ex/basic-labels-kn        ex/basic-labels-ttl)
  (test-kn-ttl-roundtrip ex/anonymous-subjects-kn  ex/anonymous-subjects-ttl)
  (test-kn-ttl-roundtrip ex/basic-annotations-kn   ex/basic-annotations-ttl)
  (test-kn-ttl-roundtrip ex/nested-annotations-kn  ex/nested-annotations-ttl))
