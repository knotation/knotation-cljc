(ns org.knotation.clj-api
  (:refer-clojure :exclude [read-string])
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [org.knotation.util :as util]
            [org.knotation.jena :as jena]
            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]
            [org.knotation.format :as fm]
            [org.knotation.kn :as kn]
            [org.knotation.tsv :as tsv]
            [org.knotation.ttl :as ttl]))

; For Apache Jena's preferred file extnesions see
; https://jena.apache.org/documentation/io/#command-line-tools

(defn path-format
  "Given a path string, return a format keyword or nil."
  [path]
  (cond
    (string/ends-with? path ".kn")  :kn
    (string/ends-with? path ".tsv") :tsv
    (string/ends-with? path ".nt")  :nt
    (string/ends-with? path ".ttl") :ttl
    (string/ends-with? path ".rdf") :rdfxml
    (string/ends-with? path ".owl") :rdfxml
    (string/ends-with? path ".edn") :edn
    :else nil))

; Environments

(def default-env (en/add-prefix en/default-env "ex" (rdf/ex)))

(defn collect-env
  [states]
  (reduce
   (fn [env state]
     (st/update-env env state))
   {}
   states))

(defn collect-prefixes
  [states]
  (reduce
   (fn [env {:keys [prefix iri base] :as state}]
     (cond
       (and prefix iri) (en/add-prefix env prefix iri)
       base (en/add-base env base)
       :else env))
   {}
   states))

; Read Input
(defn read-input
  "Given a format keyword, an initial environment (or nil), and an input-stream,
   return a lazy sequence of state maps."
  [fmt env input]
  (case fmt
    (:nt :ttl :rdfxml) (jena/read-triples (name fmt) input)
    (:kn :tsv)  (fm/read-lines fmt env (line-seq (io/reader input)))
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
  (->> paths
       (reductions
        (fn [[env _] path]
          (let [states (read-path fmt env path)]
            [(::en/env (last states)) states]))
        [nil []])
       rest
       (mapcat second)))

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
    :ttl
    (with-open [w (java.io.PrintWriter. output)]
      (doseq [o (->> states
                     rdf/assign-stanzas
                     (ttl/render-stanzas (collect-prefixes states))
                     flatten)]
        (.print w o))
      (.print w "\n"))
    ;else
    (throw (Exception. (format "Unsupported write format '%s'" fmt)))))

(defn render-string
  "Given a format keyword, an initial environment (or nil),
   and a sequence of state maps,
   return a string."
  [fmt env states]
  (let [fmt (or fmt :ttl)
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
