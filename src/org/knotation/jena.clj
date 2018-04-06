(ns org.knotation.jena
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
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
  (or (when (instance? Lang format) format)
      (when (string? format) (RDFLanguages/nameToLang format))
      (when (string? format) (RDFLanguages/contentTypeToLang ^String format))
      (when (string? format) (RDFLanguages/filenameToLang format))
      (throw (Exception. (str "Could not determine format: " format)))))

(defn read-triple
  "Given a Triple, return an RDF map with just the required values."
  [^Triple triple]
  (let [s (.getSubject triple)
        p (.getPredicate triple)
        o (.getObject triple)]
    (merge
     (when (instance? Node_URI s)
       {:si (.getURI s)})
     (when (instance? Node_Blank s)
       {:sb (str "_:" (.getLabelString (.getBlankNodeId s)))})
     (when (instance? Node_URI p)
       {:pi (.getURI p)})
     (when (instance? Node_URI o)
       {:oi (.getURI o)})
     (when (instance? Node_Blank o)
       {:ob (str "_:" (.getLabelString (.getBlankNodeId o)))})
     (when (instance? Node_Literal o)
       (merge
        {:ol (.getLiteralLexicalForm o)}
        (when-let [di (.getLiteralDatatypeURI o)]
          (when-not (= di "http://www.w3.org/1999/02/22-rdf-syntax-ns#langString")
            {:di di}))
        (when-not (string/blank? (.getLiteralLanguage o))
          {:ln (.getLiteralLanguage o)}))))))

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
      (.put queue (read-triple triple)))
    (^void quad   [_ ^Quad quad])
      ; TODO: read-quad
      ;(.put queue quad))
    (^void base   [_ ^String base]
      (.put queue {:base base}))
    (^void prefix [_ ^String prefix ^String iri]
      (.put queue {:prefix prefix :iri iri}))
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

(defn read-triples
  "Given a path to an RDF/OWL file, return a lazy sequence of RDF maps."
  [^String fmt ^InputStream input]
  (let [^BlockingQueue queue (LinkedBlockingQueue. 10000)]
    (.start (Thread. #(RDFDataMgr/parse (make-stream queue) input (get-format fmt))))
    (queue->lazy-seq queue)))
