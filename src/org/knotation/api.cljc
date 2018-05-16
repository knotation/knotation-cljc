(ns org.knotation.api
  (:require [clojure.string :as string]
            [org.knotation.environment :as env]
            [org.knotation.link :as ln]
            [org.knotation.format :as fmt]))

;; Environments
(def add-base env/add-base)
(def add-prefix env/add-prefix)
(def add-label env/add-label)
(def set-datatype env/set-datatype)
(def set-language env/set-language)
(def set-template-content env/set-template-content)
(def default-env env/default-env)

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
(defn env-of
  [h]
  (::env/env (last h)))

;; Processing to/from hub
(defn read-lines
  ([format lines] (read-lines format default-env lines))
  ([format env lines] (fmt/read-lines format env lines)))

(defn read-string
  ([format string] (read-lines format (string/split-lines string)))
  ([format env string] (read-lines format env (string/split-lines string))))

(defn read-strings
  ([format strings] (read-strings format default-env strings))
  ([format env strings]
   (reduce
    (fn [hub s]
      (concat hub (read-string format (env-of hub) s)))
    strings)))

(defn render-to
  ([format h] (render-to format (env-of h) h))
  ([format env h]
   (fmt/render-lines (fmt/render-states format env h))))
