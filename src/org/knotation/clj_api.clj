(ns org.knotation.clj-api
  (:refer-clojure :exclude [read-string])
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [org.knotation.util :as util]
            [org.knotation.jena :as jena]
            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]
            [org.knotation.kn :as kn]
            [org.knotation.tsv :as tsv]
            [org.knotation.ttl :as ttl]
            [org.knotation.api :as api]))

; For Apache Jena's preferred file extnesions see
; https://jena.apache.org/documentation/io/#command-line-tools

(defn path-format
  "Given a path string, return a format keyword or nil."
  [path]
  (when (string? path)
    (cond
      (string/ends-with? path ".kn")  :kn
      (string/ends-with? path ".tsv") :tsv
      (string/ends-with? path ".nt")  :nt
      (string/ends-with? path ".ttl") :ttl
      (string/ends-with? path ".rdf") :rdfxml
      (string/ends-with? path ".owl") :rdfxml
      (string/ends-with? path ".edn") :edn
      :else nil)))

(defn read-input
  "Given a format keyword,
   an initial state (or nil for the default state),
   and an input stream,
   return a lazy sequence of states."
  [input-format initial-state input-stream]
  (let [initial-state (or initial-state st/default-state)]
    (case input-format
      (:nt :ttl :rdfxml) (jena/read-input input-format initial-state input-stream)
      :kn (kn/read-lines initial-state (line-seq (io/reader input-stream)))
      (throw (Exception. (format "Unsupported read format '%s'" input-format))))))

(defn read-string
  "Given a format keyword,
   an initial state (or nil for the default state),
   and a content string
   return a lazy sequence of state maps."
  [input-format initial-state content]
  (try
    (read-input
     input-format
     initial-state
     (java.io.ByteArrayInputStream. (.getBytes content "UTF-8")))
    (catch Exception e
      (throw
       (Exception.
        (format
         "Failed to read from string '%s'"
         (if (< (count content) 100)
           content
           (str (subs content 0 100) " ...")))
        e)))))

(defn read-path
  "Given a format keyword (or nil to detect the format),
   an initial state (or nil for the default state),
   and a file path (string),
   return a lazy sequence of states."
  [force-format initial-state path]
  (try
    (read-input
     (or force-format (path-format path))
     (or initial-state st/default-state)
     (io/input-stream path))
    (catch Exception e
      (throw
       (Exception.
        (format "Failed to read from path '%s'" path)
        e)))))

(defn read-paths
  "Given a format keyword (or nil to detect the format),
   initial state (or nil for the default state),
   and a sequence of paths (strings),
   read each path in order, accumulating state,
   and returning a sequence of states."
  [force-format initial-state paths]
  (->> paths
       (reductions
        (fn [previous-states path]
          (read-path force-format (last previous-states) path))
        [(or initial-state st/default-state)])
       rest
       (mapcat identity)))

; Render Output

(defn render-output
  "Given a format keyword, an initial environment (or nil),
   a sequence of state maps, and an output-stream,
   write strings to the output stream."
  [fmt env states output]
  (case fmt
    :edn
    (with-open [w (java.io.PrintWriter. output)]
      (doseq [state states]
        (.println w (pr-str state))))
    :ttl
    (with-open [w (java.io.PrintWriter. output)]
      (.print w (api/render-to :ttl env states)))
    :kn
    (with-open [w (java.io.PrintWriter. output)]
      (.print w (api/render-to :kn env states)))
    ;else
    (throw (Exception. (format "Unsupported write format '%s'" fmt)))))

(def render-string
  "Given a format keyword, an initial environment (or nil),
   and a sequence of state maps,
   return a string."
  api/render-to)

(defn render-file
  "Given a format keyword, an initial environment (or nil),
   a sequence of state maps, and a file,
   write strings to the file."
  [fmt env states file]
  (let [fmt (or fmt :ttl)]
    (try
      (render-output fmt env states (io/output-stream file))
      (catch Exception e
        (throw (Exception. (format "Failed to render to file '%s'" file) e))))))

(defn render-path
  "Given a format keyword, an initial environment (or nil),
   a sequence of state maps, and a file path string,
   write strings to the file."
  [fmt env states path]
  (let [fmt (or fmt (path-format path))]
    (try
      (render-output fmt env states (io/output-stream path))
      (catch Exception e
        (throw (Exception. (format "Failed to render to path '%s'" path) e))))))
