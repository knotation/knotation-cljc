(ns org.knotation.cli
  (:require [clojure.pprint :as pp]
            [org.knotation.util :refer [throw-exception]]
            [org.knotation.state :as st]
            [org.knotation.api :as api])
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

(defn get-format
  [path]
  (cond
    (re-find #"\.kn$" path) :kn
    (re-find #"\.tsv$" path) :tsv
    (re-find #"\.nq$" path) :nq
    :else nil))

(defn lex
  [arg]
  (if (.startsWith arg "-")
    (let [[_ flag value] (re-matches #"(-\S+)=(.*)" arg)
          flag (or flag arg)]
      [(case flag
         ("-e" "--env") {::flag :env ::args 1}
         ("-p" "--prefixes") {::flag :prefixes ::args 1}
         ("-d" "--data") {::flag :data ::args 1}
         ("-f" "--format") {::flag :format ::args 1}
         ("-s" "--sort") {::flag :sort ::args 0}
         ("--sbn" "--sequential-blank-nodes")
         {::flag :sequential-blank-nodes ::args 0}
         ("-E" "--reset-env") {::flag :reset-env ::args 0}
         ("--dump") {::flag :dump ::args 0}
         (throw-exception (str "Unknown option: " arg)))
       (when value {::value value})])
    [{::value arg}]))

(defn lexer
  [args]
  (->> args
       (mapcat lex)
       (remove nil?)))

(defn group-args
  [args]
  (loop [done []
         todo (lexer args)]
    (let [{:keys [::args] :or {args 0} :as next} (first todo)]
      (if-not next
        done
        (recur
         (->> todo
              (take (inc args))
              (map #(dissoc % ::args))
              (apply merge)
              (conj done))
         (drop (inc args) todo))))))

(defn operation
  [{:keys [::flag ::value] :as arg}]
  (case flag
    nil
    {::api/operation-type :read
     ::st/source value
     ::st/format (get-format value)
     ::st/lines (line-seq (clojure.java.io/reader value))}

    :env
    {::api/operation-type :read-env
     ::st/source value
     ::st/format (get-format value)
     ::st/lines (line-seq (clojure.java.io/reader value))}

    :prefixes
    {::api/operation-type :read-prefixes
     ::st/source value
     ::st/format (get-format value)
     ::st/lines (line-seq (clojure.java.io/reader value))}

    :data
    {::api/operation-type :read-data
     ::st/source value
     ::st/format (get-format value)
     ::st/lines (line-seq (clojure.java.io/reader value))}

    :format
    {::api/operation-type :render
     ::st/format (keyword value)}

    :sort
    {::api/operation-type :sort}

    :sequential-blank-nodes
    {::api/operation-type :sequential-blank-nodes}

    :reset-env
    {::api/operation-type :reset-env}

    :dump
    {::api/operation-function
     (fn [states]
       (map #(do (println %) %) states))}))

(defn operations
  [args]
  (let [operations (->> args group-args (map operation))]
    (if (->> operations
             (filter #(= :render (::api/operation-type %)))
             first)
      operations
      (concat
       operations
       [{::api/operation-type :render}]))))

(defn print-lines
  [states]
  (doseq [state states]
    (when-let [message (-> state ::st/error ::st/error-message)]
      (throw-exception (str "ERROR: " message)))
    (when-let [lines (-> state ::st/output ::st/lines)]
      (doseq [line lines]
        (println line)))))

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
         operations
         api/run-operations
         print-lines)
    (catch Exception e
      (println e) ; TODO: remove
      (println usage)
      (System/exit 1))))
