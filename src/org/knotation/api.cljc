(ns org.knotation.api
  (:require [clojure.string :as string]

            [org.knotation.util :as util]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]
            [org.knotation.format :as fmt]

            [org.knotation.kn :as kn]
            org.knotation.ttl
            org.knotation.nq
            org.knotation.tsv))

;; Environments
(def add-base en/add-base)
(def add-prefix en/add-prefix)
(def add-label en/add-label)
(def set-datatype en/set-datatype)
(def set-language en/set-language)
(def set-template-content en/set-template-content)
(def blank-env en/blank-env)
(def default-env en/default-env)

(defn labels [env] (::en/label-seq env))
(defn prefixes [env] (::en/prefix-seq env))
(defn prefix-states [env]
  (map (fn [[prefix iri]]
         {::st/event ::st/prefix
          :prefix prefix
          :iri iri})
       (::en/prefix-iri env)))

(def find-prefix en/find-prefix)
(def name->iri en/name->iri)
(def iri->name en/iri->name)
(def iri->curie en/iri->curie)
(def iri->label en/iri->label)

;; State queries
(defn graph-end? [s] (= ::st/graph-end (::st/event s)))
(defn error? [s] (= ::st/error (::st/event s)))

(defn error-message [s] (->> s ::st/error ::st/error-message))
(defn error-type [s] (->> s ::st/error ::st/error-type))

(defn line-num-in [s] (->> s :input :line-number))
(defn line-ct-in [s] (->> s :input :line-count))

(defn line-num-out [s] (->> s :output :line-number))
(defn line-ct-out [s] (->> s :output :line-count))

;; State collection queries
(defn env-of
  [h]
  (::en/env (last h)))

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

;; Processing to/from state
(defn -inner-read-parses
  "Given a format keyword, and initial environment (or nil), and a sequence of parses,
   return a lazy sequence of [env parse state] triples."
  [fmt env parses]
  (->> parses
       (reductions
        (fn [previous parse]
          (let [[previous-env _ previous-state] (last previous)
                previous-env (or previous-env en/blank-env)
                previous-state (or previous-state st/blank-state)
                env (st/update-env previous-env previous-state)
                state (fmt/read-parse fmt env parse)
                [state expanded-parses] (fmt/expand-state fmt env state)]
            (concat
             [[env parse state]]
             (-inner-read-parses fmt env expanded-parses))))
        [[env nil nil]])
       rest
       (mapcat identity)))

(defn read-lines
  ([format lines]
   (read-lines format blank-env lines))
  ([format env lines]
   ; TODO: more formats
   (kn/read-lines env lines)))

(defn read-from
  ([format thing] (read-from format blank-env thing))
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

(defn collect-line-map
  [state]
  (->> state
       (map (fn [s]
              [[(get-in s [:input :line-number] 0) (get-in s [:input :line-count] 0)]
               [(get-in s [:output :line-number] 0) (get-in s [:output :line-count] 0)]]))
       (map (fn [[[ln-in ct-in] [ln-out ct-out]]]
              (let [out (set (take ct-out (drop ln-out (range))))]
                (map
                 (fn [in] [in out])
                 (take ct-in (drop ln-in (range)))))))
       dedupe
       (apply concat)))

(defn line-map-of
  ([format h] (line-map-of format (env-of h) h))
  ([format env h] (collect-line-map (fmt/render-states format env h))))
