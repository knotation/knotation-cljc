(ns org.knotation.api
  (:require [#?(:clj clojure.pprint :cljs cljs.pprint) :as pp]
            [clojure.string :as string]
            [org.knotation.util :as util]
            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]
            [org.knotation.format :as fm]
            [org.knotation.kn]
            [org.knotation.tsv]
            [org.knotation.nq]
            [org.knotation.ttl]
            [org.knotation.rdfa]))

;; The API works in terms of operations.
;; Each operation is converted into a function
;; that takes a sequence of states (usually lazy)
;; and returns a sequence of states (usually lazy).
;; The functions are composed and run on a sequence
;; consisting of a single blank state.
;; This is similar to a stack of Ring middleware.
;;
;; We have three main kinds of operations:
;; - read operations concatenate a number of states
;; - render operations render each input state to an output state
;; - other operations operate on the sequence in arbitrary ways
;;
;; The read and render functions are specific to the format
;; and are looked up in the @fm/formats atom.

(def example-read-operation
  {::operation-type :read
   ::st/format :kn
   ::st/lines ["@prefix ex: <https://example.com/>" ""]})

(def example-render-operation
  {::operation-type :render
   ::st/format :nq})

(defn input
  [format content]
  {::operation-type :read
   ::st/format format
   ::st/line-number 1
   ::st/lines (string/split-lines content)})

(defn env
  [format content]
  {::operation-type :read-env
   ::st/format format
   ::st/line-number 1
   ::st/lines (string/split-lines content)})

(defn output
  [format]
  {::operation-type :render
   ::st/format format})

(def prefixes {::operation-type :prefixes})

(def space {::operation-type :space})

(defn take-while+
  [pred coll]
  (lazy-seq
   (when-let [[f & r] (seq coll)]
     (if (pred f)
       (cons f (take-while+ pred r))
       [f]))))

(defn operation-function
  [{:keys [::operation-type ::operation-function
           ::st/format ::st/lines]
    :as operation}]
  (cond
    operation-function
    operation-function

    (= operation-type :reset-env)
    (fn [states]
      (map #(assoc % ::en/env en/blank-env ::en/env-before en/blank-env) states))

    (= operation-type :space)
    (fn [states]
      (concat
       states
       [{::st/event ::st/space
         ::en/env (-> states last ::en/env)}]))

    (= operation-type :prefixes)
    (fn [states]
      (for [{:keys [::st/event] :as state} states]
        (if (= ::st/prefix event)
          (dissoc state ::st/mode)
          state)))

    (= operation-type :stop-on-error)
    (fn [states]
      (take-while+ #(not (::st/error %)) states))

    (= operation-type :sequential-blank-nodes)
    fm/sequential-blank-nodes

    (= operation-type :read)
    (fm/read-function format lines)

    (= operation-type :read-env)
    (fm/read-env-function format lines)

    (= operation-type :read-data)
    (fm/read-data-function format lines)

    (= operation-type :render)
    (fm/render-function format)

    :else
    (fn [states]
      (concat
       states
       [{:st/error
         {:st/error-type :unknown-operation-type
          ::st/error-message
          (str "Unknown operation type: " operation-type)}}]))))

(defn run-operations
  [operations]
  ((->> operations
        (map operation-function)
        reverse
        (apply comp))
   [st/blank-state]))

(defn content
  [states]
  (string/join
   "\n"
   (reduce
    (fn [lines state]
      (concat lines (-> state ::st/output ::st/lines)))
    []
    states)))

(defn errors
  [states]
  (string/join
   "\n"
   (reduce
    (fn [lines state]
      (if-let [message (-> state ::st/error ::st/error-message)]
        (conj lines message)
        lines))
    []
    states)))
