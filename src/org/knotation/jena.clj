(ns org.knotation.jena
  (:refer-clojure :exclude [read-string])
  (:require [clojure.java.io :as io]
            [clojure.string :as string]

            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]
            [org.knotation.format :as fmt])
  (:import (java.io InputStream ByteArrayInputStream)
           (java.util.concurrent BlockingQueue LinkedBlockingQueue TimeUnit)
           (org.apache.jena.graph Triple Node_URI Node_Blank Node_Literal)
           (org.apache.jena.sparql.core Quad)
           (org.apache.jena.riot RDFDataMgr RDFLanguages Lang)
           (org.apache.jena.riot.lang PipedTriplesStream PipedRDFIterator)
           (org.apache.jena.riot.system StreamRDF)))

(set! *warn-on-reflection* true)

(defn get-format
  "Given a Lang, a format string, a content type, or a filename,
   try to return an RDF Lang (file format)."
  ^Lang
  [format]
  (let [format (if (keyword? format) (name format) format)]
    (or (when (instance? Lang format) format)
        (when (string? format) (RDFLanguages/nameToLang format))
        (when (string? format) (RDFLanguages/nameToLang (string/upper-case format)))
        (when (string? format) (RDFLanguages/contentTypeToLang ^String format))
        (when (string? format) (RDFLanguages/filenameToLang format))
        (throw (Exception. (str "Could not determine format: " format))))))

(defn read-triple
  "Given a Triple, return an RDF map with just the required values."
  [^Triple triple]
  (let [s (.getSubject triple)
        p (.getPredicate triple)
        o (.getObject triple)]
    (merge
     (when (instance? Node_URI s)
       {::rdf/si (.getURI s)})
     (when (instance? Node_Blank s)
       {::rdf/sb (str "_:" (.getLabelString (.getBlankNodeId s)))})
     (when (instance? Node_URI p)
       {::rdf/pi (.getURI p)})
     (when (instance? Node_URI o)
       {::rdf/oi (.getURI o)})
     (when (instance? Node_Blank o)
       {::rdf/ob (str "_:" (.getLabelString (.getBlankNodeId o)))})
     (when (instance? Node_Literal o)
       (merge
        {::rdf/ol (.getLiteralLexicalForm o)}
        (when-let [di (.getLiteralDatatypeURI o)]
          (when-not (= di "http://www.w3.org/1999/02/22-rdf-syntax-ns#langString")
            {::rdf/di di}))
        (when-not (string/blank? (.getLiteralLanguage o))
          {::rdf/lt (.getLiteralLanguage o)}))))))

(defn make-stream
  "Given a BlockingQueue, return an instance of the StreamRDF interface
   that will convert the inputs to states
   and put them on the queue."
  ^StreamRDF
  [^BlockingQueue queue]
  (reify StreamRDF
    (^void start  [_]
      (.put queue :start))
    (^void triple [_ ^Triple triple]
      (.put queue {::st/event ::st/statement ::rdf/quad (read-triple triple)}))
    (^void quad   [_ ^Quad quad])
      ; TODO: read-quad
      ;(.put queue quad))
    (^void base   [_ ^String base]
      (.put queue {::st/event ::st/base :base base}))
    (^void prefix [_ ^String prefix ^String iri]
      (.put queue {::st/event ::st/prefix :prefix prefix :iri iri}))
    (^void finish [_]
      (.put queue :finish))))

; This implementation of read-triples with a BlockingQueue
; is based on org.apache.jena.riot.lang.PipedRDFIterator

(defn queue->lazy-seq
  "Given a queue used with make-stream,
   return a lazy-sequence of states.
   Waits one second before throwing InterruptedException."
  [^BlockingQueue queue]
  (lazy-seq
   (let [item (.poll queue 1000 TimeUnit/MILLISECONDS)]
     (when (instance? Throwable item)
       (throw item))
     (case item
       :finish []
       :start (queue->lazy-seq queue)
       (cons item (queue->lazy-seq queue))))))

(defn insert-stanza-events
  [states]
  (->> states
       (partition-by #(get-in % [::rdf/quad ::rdf/zn]))
       (mapcat
        (fn [states]
          (let [zn (-> states first ::rdf/quad ::rdf/zn)]
            (if zn
              (concat
               [{::st/event ::st/stanza-start :subject zn}]
               states
               [{::st/event ::st/stanza-end :subject zn}])
              states))))))

(defn read-input
  "Given a format string and an input stream for RDF data,
   return a lazy sequence of RDF triple maps."
  [^String fmt ^InputStream input]
  (let [^BlockingQueue queue (LinkedBlockingQueue. 10000)]
    (.start (Thread. #(RDFDataMgr/parse (make-stream queue) input (get-format fmt))))
    (->> queue
         queue->lazy-seq
         st/assign-stanzas
         insert-stanza-events)))

(defn read-string
  "Given a format string and an input string of RDF data,
   return a lazy sequence of RDF triple maps."
  [^String fmt ^String input]
  (read-input fmt (java.io.ByteArrayInputStream. (.getBytes input "UTF-8"))))
