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
            [org.knotation.nq :as nq]))

(def fail-on-error (atom true))

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

(defn get-lines
  "Given an input stream, return the sequence of lines without the line 
  terminators stripped."
  [input-stream]
  (->> input-stream
       io/reader
       line-seq
       (map #(str % "\n"))))

(defn read-input
  "Given a format keyword,
   an initial state (or nil for the default state),
   and an input stream,
   return a lazy sequence of states."
  [input-format initial-state input-stream]
  (let [initial-state (or initial-state st/default-state)]
    (case input-format
      (:nt :ttl :rdfxml) (jena/read-input input-format initial-state input-stream)
      :kn (kn/read-lines initial-state (get-lines input-stream))
      :tsv (tsv/read-lines initial-state (get-lines input-stream))
      (throw (Exception. (format "Unsupported read format '%s'" input-format))))))

(defn read-string
  "Given a format keyword,
   an initial state (or nil for the default state),
   and a content string
   return a lazy sequence of state maps."
  [input-format initial-state content]
  (let [states (read-input 
                 input-format 
                 initial-state 
                 (java.io.ByteArrayInputStream. (.getBytes content "UTF-8")))]
    (when-let [errors (and @fail-on-error (st/filter-errors states))]
      (println
        (format
         "Failed to read from string '%s' due to %d error(s):\n\t%s"
         (if (< (count content) 50)
           content
           (str (subs content 0 50) " ..."))
         (count errors)
         (st/join-errors errors)))
      (System/exit 1))
    states))

(defn read-path
  "Given a format keyword (or nil to detect the format),
   an initial state (or nil for the default state),
   and a file path (string),
   return a lazy sequence of states."
  [force-format initial-state path]
  (let [states (read-input
                (or force-format (path-format path))
                (or initial-state st/default-state)
                (io/input-stream path))]
    (when-let [errors (and @fail-on-error (st/filter-errors states))]
      (println
        (format 
          "Failed to read from '%s' due to %d error(s):\n\t%s" 
          path 
          (count errors)
          (st/join-errors errors)))
      (System/exit 1))
    states))

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

(defn render-states
  "Given a format keyword, an initial state (or nil),
   a sequence of state maps,
   return a sequence of rendered states."
  [fmt initial-state states]
  (let [initial-state (or initial-state st/default-state)]
    (case fmt
      :nt (nq/render-states states)
      :ttl (ttl/render-states initial-state states)
      :kn (kn/render-states initial-state states)
      (throw (Exception. (format "Unsupported write format '%s'" fmt))))))

(defn render-output
  "Given a format keyword, an initial state (or nil),
   a sequence of state maps, and an output-stream,
   write strings to the output stream."
  [fmt initial-state states output]
  (with-open [w (java.io.PrintWriter. output)]
    (if (= :edn fmt)
      (doseq [state states]
        (.println w (pr-str state)))
      (doseq [line (st/render-output (render-states fmt initial-state states))]
        (.print w line)))))

(defn render-string
  "Given a format keyword, an initial state (or nil),
   and a sequence of state maps,
   return a string."
  [fmt initial-state states]
  (let [string-writer (java.io.StringWriter.)]
    (render-output fmt initial-state states string-writer)
    (.toString string-writer)))

(defn render-file
  "Given a format keyword, an initial state (or nil),
   a sequence of state maps, and a file,
   write strings to the file."
  [fmt initial-state states file]
  (try
    (render-output fmt initial-state states (io/output-stream file))
    (catch Exception e
      (throw (Exception. (format "Failed to render to file '%s'" file) e)))))

(defn render-path
  "Given a format keyword (or nil to detect), an initial state (or nil),
   a sequence of state maps, and a file path string,
   write strings to the file."
  [fmt initial-state states path]
  (let [fmt (or fmt (path-format path))]
    (try
      (render-output fmt initial-state states (io/output-stream path))
      (catch Exception e
        (throw (Exception. (format "Failed to render to path '%s'" path) e))))))
