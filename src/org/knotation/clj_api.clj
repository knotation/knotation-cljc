(ns org.knotation.clj-api
  (:refer-clojure :exclude [read-string])
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [org.knotation.util :as util]
            [org.knotation.jena :as jena]))

; For Apache Jena's preferred file extnesions see
; https://jena.apache.org/documentation/io/#command-line-tools

(defn path-format
  "Given a path string, return a format keyword or nil."
  [path]
  (cond
    (util/ends-with? path ".nt")  :nt
    (util/ends-with? path ".ttl") :ttl
    (util/ends-with? path ".rdf") :rdfxml
    (util/ends-with? path ".owl") :rdfxml
    (util/ends-with? path ".edn") :edn
    :else nil))

; Read Input

(defn read-input
  "Given a format keyword, an initial environment (or nil), and an input-stream,
   return a lazy sequence of state maps."
  [fmt env input]
  (case fmt
    :nt (jena/read-triples "nt" input)
    :ttl (jena/read-triples "ttl" input)
    :rdfxml (jena/read-triples "rdfxml" input)
    (throw (Exception. (format "Unsupported read format '%s'" fmt)))))

(defn read-path
  "Given a format keyword (or nil), an initial environment (or nil), and a file path,
   return a lazy sequence of state maps."
  [fmt env path]
  (let [fmt (or fmt (path-format path))]
    (try
      (read-input fmt env (io/input-stream path))
      (catch Exception e
        (throw (Exception. (format "Failed to read from path '%s'" path) e))))))

(defn read-paths
  [fmt env paths]
  ;(read-path fmt nil (first paths)))
  (mapcat (partial read-path fmt env) paths))

(defn read-string
  "Given a format keyword, an initial environment (or nil), and a content string
   return a lazy sequence of state maps."
  [fmt env content]
  (try
    (read-input fmt env (java.io.ByteArrayInputStream. (.getBytes content "UTF-8")))
    (catch Exception e
      (throw
       (Exception.
        (format
         "Failed to read from string '%s'"
         (if (< (count content) 100)
           content
           (str (subs content 0 100) " ...")))
        e)))))

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
    ;else
    (throw (Exception. (format "Unsupported write format '%s'" fmt)))))

(defn render-string
  "Given a format keyword, an initial environment (or nil),
   and a sequence of state maps,
   return a string."
  [fmt env states]
  (let [fmt (or fmt :edn)
        output (java.io.ByteArrayOutputStream.)]
    (try
      (render-output fmt env states output)
      (str output)
      (catch Exception e
        (throw (Exception. (format "Failed to render to string") e))))))

(defn render-file
  "Given a format keyword, an initial environment (or nil),
   a sequence of state maps, and a file,
   write strings to the file."
  [fmt env states file]
  (let [fmt (or fmt :edn)]
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
