(ns org.knotation.api
  (:require [clojure.string :as string]

            [org.knotation.util :as util]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]

            [org.knotation.kn :as kn]
            [org.knotation.ttl :as ttl]
            [org.knotation.nq :as nq]
            [org.knotation.tsv :as tsv]))

;; Environments
(def add-base en/add-base)
(def add-prefix en/add-prefix)
(def add-label en/add-label)
(def set-datatype en/set-datatype)
(def set-language en/set-language)
(def set-template-content en/set-template-content)
(def blank-env en/blank-env)
(def default-env en/default-env)
(def default-state st/default-state)

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

(defn line-num-in [s] (->> s ::st/input ::st/start ::st/line-number))
(defn line-ct-in [s]
  (inc
   (- (->> s ::st/input ::st/end ::st/line-number)
      (line-num-in s))))

(defn line-num-out [s] (->> s ::st/output ::st/start ::st/line-number))
(defn line-ct-out [s]
  (inc
   (- (->> s ::st/output ::st/end ::st/line-number)
      (line-num-in s))))

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

(defn read-lines
  ([format lines]
   (read-lines format default-state lines))
  ([format initial-state lines]
   ; TODO: more formats
   (kn/read-lines initial-state lines)))

(defn read-from
  ([format thing]
   (read-from format default-state thing))
  ([format initial-state thing]
   (cond
     (string? thing)
     (read-lines format initial-state (util/split-lines thing))

     ; TODO: Look more closely at this.
     (coll? thing)
     (reduce
      (fn [prev s]
        (concat prev (read-lines format (last prev) (util/split-lines s))))
      nil
      thing)

     :else (util/throw-exception "Can't read from a thing of type" (type thing)))))

(defn render-states
  [format env states]
  (case format
    :kn (kn/render-states env states)
    :ttl (ttl/render-states env states)
    :nq (nq/render-states states)
    :else (util/throw-exception "Unsupported format: " format)))

(defn render-to
  ([format states] (render-to format blank-env states))
  ([format env states]
   (st/render-output-string (render-states format env states))))

(defn collect-line-map
  [state]
  (let [line-range
        (fn [m]
          (when (::st/start m)
            (let [from (get-in m [::st/start ::st/line-number])
                  to (get-in m [::st/end ::st/line-number])]
              (range from (max
                           (if (= 0 (get-in m [::st/end ::st/column-number])) (dec to) to)
                           (inc from))))))]
    (->> state
         (filter
          #(or (get-in % [::st/input ::st/start])
               (get-in % [::st/output ::st/start])))
         (map (fn [s]
                [(line-range (::st/input s))
                 (line-range (::st/output s))]))
         (reductions
          (fn [memo elem]
            (if (first elem)
              elem
              [(first memo) (second elem)])))
         (partition-by
          (let [ix (atom 0)
                prev (atom 0)]
            (fn [[from to]]
              (let [next (when (> @prev (first from))
                           (swap! ix inc)
                           @ix)]
                (reset! prev (first from))
                next))))
         (map
          #(reduce
            (fn [memo [froms tos]]
              (reduce
               (fn [memo elem]
                 (assoc memo elem
                        (if-let [existing (get memo elem)]
                          (concat existing tos)
                          tos)))
               memo froms))
            {} %)))))

(defn line-maps-of
  ([format h] (line-maps-of format (env-of h) h))
  ([format env h] (collect-line-map (render-states format env h))))
