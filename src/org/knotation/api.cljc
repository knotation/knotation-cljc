(ns org.knotation.api
  (:require [clojure.pprint :as pp]
            [org.knotation.state :as st]
            [org.knotation.kn :as kn]
            [org.knotation.tsv :as tsv]
            [org.knotation.nq :as nq]))

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

(defn input->blocks
  "Given and input, return a lazy sequence of blocks."
  [{:keys [lines] :as input}]
  (map-indexed
   (fn [index line]
     (-> input
         (dissoc :lines)
         (assoc :block [line] :line-number (inc index))))
   lines))

(defn block->state
  "Given the previous state and a block in a given format,
   process the block and return the new state."
  [state {:keys [format] :as block}]
  (let [state (-> state
                  (dissoc :quads)
                  (merge block)
                  ; TODO: is this a good idea?
                  (assoc :env-before (:env state)))]
    (try
      (case format
        :kn (kn/block->state state)
        :tsv (tsv/block->state state)
        :nq (nq/block->state state)
        (throw (Exception. (str "Unknown input format: " format))))
      (catch Exception e
        (println "STATE" state)
        (throw e)))))

(defn process-inputs
  [{:keys [inputs] :as pipeline}]
  (->> inputs
       (mapcat input->blocks)
       (reductions block->state st/default-state)
       rest))

(defn process-outputs
  [{:keys [outputs] :as pipeline} states]
  (let [format (:format (last outputs))]
    (case format
      (nil :nq) (nq/states->lines states)
      :kn (kn/states->lines states)
      ;:ttl
      ;:rdfa
      ;:json-ld
      :env (->> states last :env pp/pprint)
      (throw (Exception. (str "Unknown output format: " format))))))

(defn process-pipeline
  [pipeline]
  (process-outputs pipeline (process-inputs pipeline)))

(def example-kn
  "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
@prefix ex: <https://example.com/>

: ex:foo
rdfs:label: Foo
ex:comment: comment")

(def example-tsv
  "@subject  label
ex:1  One
ex:2  Two")
