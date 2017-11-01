(ns org.knotation.cli
  (:require [org.knotation.api :as api])
  (:gen-class))

; The Knotation CLI converts one or more input files to an output file.
; The current implementation only handles NQuad output.
; An input file without options is processed as an both environment and data.
; The `-e` and `-d` options restrict processing to just environment or data.

(def usage
  "kn [OPTIONS] [FILES]
  -e FILE      --env FILE
  -d FILE      --data FILE
  -v           --version
  -h           --help
")

(defn version
  []
  (str
   "kn "
   (-> (eval 'org.knotation.cli)
       .getPackage
       .getImplementationVersion
       (or "DEVELOPMENT"))))

(def default-input
  {:mode :both})

(defn get-format
  [path]
  (cond
    (re-find #"\.kn$" path) :kn
    (re-find #"\.tsv$" path) :tsv
    :else nil))

(defn build-input
  [current path]
  (merge
   default-input
   current
   {:input path
    :format (get-format path)
    :lines (line-seq (clojure.java.io/reader path))}))

(defn build-pipeline
  "Given a sequence of strings,
   return a sequence of input configuration maps
   or throw an exception on invalid options."
  [args]
  (reduce
   (fn [coll arg]
     (cond
       (contains? #{"-e" "--env"} arg)
       (assoc-in coll [:current :mode] :env)
       (contains? #{"-d" "--data"} arg)
       (assoc-in coll [:current :mode] :data)
       (.startsWith arg "-")
       (throw (Exception. (str "Unknown option: " arg)))
       :else
       (-> coll
           (dissoc :current)
           (update :inputs conj (build-input (:current coll) arg)))))
   {:current {}
    :inputs []
    :outputs []}
   args))

(defn -main
  [& args]
  (try
    (when (some #{"-h" "--help"} args)
      (println usage)
      (System/exit 0))
    (when (some #{"-v" "--version"} args)
      (println (version))
      (System/exit 0))
    (->> args
         build-pipeline
         api/process-pipeline
         (map println)
         doall)
    (catch Exception e
      (println e) ; TODO: remove
      (println usage)
      (System/exit 1))))
