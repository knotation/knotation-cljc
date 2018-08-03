(ns org.knotation.api
  (:require [clojure.string :as string]

            [org.knotation.util :as util]
            [org.knotation.environment :as env]
            [org.knotation.state :as st]
            [org.knotation.link :as ln]
            [org.knotation.format :as fmt]

            org.knotation.kn
            org.knotation.ttl
            org.knotation.nq
            org.knotation.tsv))

;; Environments
(def add-base env/add-base)
(def add-prefix env/add-prefix)
(def add-label env/add-label)
(def set-datatype env/set-datatype)
(def set-language env/set-language)
(def set-template-content env/set-template-content)
(def default-env env/default-env)

(defn labels [env] (::env/label-seq env))
(defn prefixes [env] (::env/prefix-seq env))

(def find-prefix ln/find-prefix)
(def ->iri ln/->iri)
(def iri->name ln/iri->name)
(def iri->curie ln/iri->curie)
(def iri->label ln/iri->label)

;; Hub format manipulation
;;; I'm calling it "hub", because this is the internal representation
;;; that each of the spokes eventually comes down to. I think we can
;;; simplify it a great deal once we figure out what we want the
;;; external interface to look like

;; Hub state queries
(defn graph-end? [s] (= :graph-end (:event s)))
(defn error? [s] (= :error (:event s)))

(defn error-message [s] (->> s :error :error-message))
(defn error-type [s] (->> s :error :error-type))

(defn line-num-in [s] (->> s :input :line-number))
(defn lines-in [s] (->> s :input :lines))

(defn line-num-out [s] (->> s :output :line-number))
(defn lines-out [s] (->> s :output :lines))

;; Hub collection queries
(defn env-of
  [h]
  (::env/env (last h)))

(defn errors-of
  [h]
  (map
   (fn [s]
     (when (error? s)
       [(error-type s) (error-message s)]))
   h))

(defn any-errors?
  [h]
  (->> h errors-of (remove nil?) empty? not))

;; Processing to/from hub
(defn read-lines
  ([format lines] (read-lines format default-env lines))
  ([format env lines] (fmt/read-lines format env lines)))

(defn read-from
  ([format thing] (read-from format default-env thing))
  ([format env thing]
   (cond
     (string? thing)
     (read-lines format env (util/split-lines thing))

     (coll? thing)
     (reduce
      (fn [prev s]
        (concat prev (read-lines format (or (env-of prev) env) (util/split-lines s))))
      nil thing)

     :else (util/throw-exception "Can't read from a thing of type" (type thing)))))

(defn render-to
  ([format h] (render-to format (env-of h) h))
  ([format env h]
   (fmt/render-output (fmt/render-states format env h))))
