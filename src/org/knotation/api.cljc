(ns org.knotation.api
  (:require [#?(:clj clojure.pprint :cljs cljs.pprint) :as pp]
            [org.knotation.util :as util]
            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]
            [org.knotation.kn :as kn]
            [org.knotation.tsv :as tsv]
            [org.knotation.nq :as nq]
            [org.knotation.rdfa :as rdfa]))

;; The Knotation API deals with:
;;
;; 1. input: A map describing a file or string of content.
;;    The content string is split into a lazy sequence of lines.
;; 2. block: A map with a vector of strings to process as a unit
;;    (i.e. lines or cells) and location information from the input.
;; 3. state: A map for a processed block, including location,
;;    an environment, and a list of output quads.
;; 4. quad: A map representing the processed data as an RDF quad.
;;
;; We lazily turn inputs into blocks, then reduce to generate states,
;; and extract the quads.
;; The last step is usually to turn the quads back into strings.

; TODO: Format implemenations should group their own lines.
(defn group-lines
  "Given and input, return a lazy sequence of blocks."
  [{:keys [::st/lines ::st/line-number]
    :or {line-number 1}
    :as input}]
  (map-indexed
   (fn [index line]
     (-> input
         (assoc ::st/lines [line]
                ::st/line-number (+ line-number index))))
   lines))

(defn process-input
  "Given the previous state and a block in a given format,
   process the block and return the new state."
  [state {:keys [::st/format] :as input}]
  (let [state (-> state
                  (dissoc ::rdf/quads)
                  (assoc ::st/input input)
                  (assoc ::en/env-before (::en/env state)))]
    (case format
      :kn (kn/block->state state)
      :tsv (tsv/block->state state)
      :nq (nq/block->state state)
      (assoc
       state
       ::st/error
       {::st/error-type :unknown-input-format
        ::st/error-message (str "Unknown input format: " format)}))))

(defn process-inputs
  [{:keys [::inputs] :as pipeline}]
  (->> inputs
       (mapcat group-lines)
       (reductions process-input st/blank-state)
       rest))

(defn process-output
  [{:keys [::output] :as pipeline} states]
  (let [format (::st/format output)]
    (case format
      (nil :nq) (nq/process-outputs states)
      :kn (kn/process-outputs states)
      :rdfa (rdfa/process-outputs states)
      ;:ttl
      ;:json-ld
      :env (->> states last :env pp/pprint)
      (util/throw-exception "Unknown output format:" format))))

(defn process-pipeline
  [pipeline]
  (process-output pipeline (process-inputs pipeline)))

(defn kn
  [content]
  {::st/format :kn
   ::st/line-number 1
   ::st/lines (clojure.string/split-lines content)})

(defn content
  [states]
  (clojure.string/join
   \n
   (reduce
    (fn [lines state]
      (concat lines (-> state ::st/output ::st/lines)))
    []
    states)))

(defn errors
  [states]
  (clojure.string/join
   \n
   (reduce
    (fn [lines state]
      (if-let [message (-> state ::st/error ::st/error-message)]
        (conj lines message)
        lines))
    []
    states)))
