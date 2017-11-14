(ns org.knotation.cli
  (:require [org.knotation.state :as st]
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

(def default-input
  {::st/mode :both})

(defn build-input
  [current path]
  (merge
   default-input
   current
   {::st/source path
    ::st/format (get-format path)
    ::st/line-number 1
    ::st/lines (line-seq (clojure.java.io/reader path))}))

(defn lex
  [arg]
  (if (.startsWith arg "-")
    (let [[_ flag value] (re-matches #"(-\S+)=(.*)" arg)
          flag (or flag arg)]
      [(case flag
         ("-e" "--env") {:flag :env :args 1}
         ("-d" "--data") {:flag :data :args 1}
         ("-f" "--format") {:flag :format :args 1}
         ("-s" "--sort") {:flag :sort :args 0}
         (throw (Exception. (str "Unknown option: " arg))))
       (when value {:value value})])
    [{:value arg}]))

(defn lexer
  [args]
  (->> args
       (mapcat lex)
       (remove nil?)))

(defn group-args
  [args]
  (loop [done []
         todo (lexer args)]
    (let [{:keys [args] :or {args 0} :as next} (first todo)]
      (println args)
      (if-not next
        done
        (recur
         (conj done (take (inc args) todo))
         (drop (inc args) todo))))))

(defn build-pipeline
  "Given a sequence of strings,
   return a sequence of input configuration maps
   or throw an exception on invalid options."
  [args]
  (reduce
   (fn [coll arg]
     (cond
       (contains? #{"-e" "--env"} arg)
       (assoc-in coll [:current ::st/mode] :env)

       (contains? #{"-d" "--data"} arg)
       (assoc-in coll [:current ::st/mode] :data)

       (= "--format=env" arg)
       (update coll ::api/outputs conj {::st/format :env})

       (= "--format=nq" arg)
       (update coll ::api/outputs conj {::st/format :nq})

       (= "--format=kn" arg)
       (update coll ::api/outputs conj {::st/format :kn})

       (= "--format=rdfa" arg)
       (update coll ::api/outputs conj {::st/format :rdfa})

       (.startsWith arg "-")
       (throw (Exception. (str "Unknown option: " arg)))

       :else
       (-> coll
           (dissoc :current)
           (update ::api/inputs conj (build-input (:current coll) arg)))))
   {:current {}
    ::api/inputs []
    ::api/outputs []}
   args))

(defn stop-on-error
  [state]
  (if-let [message (-> state ::st/error ::st/error-message)]
    (throw (Exception. (str "ERROR: " message)))
    state))

(defn print-lines
  [state]
  (if-let [lines (-> state ::st/output ::st/lines)]
    (doseq [line lines]
      (println line))
    state))

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
         (map stop-on-error)
         (map print-lines)
         doall)
    (catch Exception e
      (println e) ; TODO: remove
      (println usage)
      (System/exit 1))))
