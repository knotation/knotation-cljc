(ns org.knotation.api
  (:require [clojure.string :as string]
            [org.knotation.environment :as env]
            [org.knotation.link :as ln]
            [org.knotation.format :as fmt]))

(defmacro pull [ns vlist]
  `(do ~@(for [i vlist]
           `(def ~i ~(symbol (str ns "/" i))))))

;; Environments
(pull env [add-base add-prefix add-label
           set-datatype set-language set-template-content
           default-env])

(pull ln [find-prefix
          label->iri curie->iri subject->iri
          iri->name iri->curie iri->label])

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

(defn render-to
  ([format h] (render-to format (env-of h) h))
  ([format env h]
   (fmt/render-lines (fmt/render-states format env h))))
