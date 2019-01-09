(ns org.knotation.cli
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.tools.cli :refer [parse-opts]]
            [org.knotation.state :as st]
            [org.knotation.clj-api :as api])
  (:gen-class))

(def usage "kn [OPTIONS] [FILES]
Input formats:  knotation, nquads, ntriples, owl (OWLXML), tsv
Output formats: turtle, knotation, nquads, ntriples, owl (OWLXML),
                rdfa, json (tree), dot
Options:
  -f FORMAT, -r FORMAT  --from=FORMAT, --read=FORMAT
  -t FORMAT, -w FORMAT  --to=FORMAT, --write=FORMAT
  -o FILENAME           --output=FILENAME
  -s                    --sequential-blank-nodes
  -v                    --version
  -h                    --help")

(defn version
  []
  (str
   "kn "
   (-> (eval 'org.knotation.cli)
       .getPackage
       .getImplementationVersion
       (or "DEVELOPMENT"))))

(def cli-options
  [["-f" "--from FORMAT" "Format for input"]
   ["-r" "--read FORMAT" "Same as --from"]
   ["-t" "--to FORMAT" "Format for output"]
   ["-w" "--write FORMAT" "Same as --to"]
   ["-o" "--output FILENAME" "File for output"]
   ["-c" "--context FILENAME" "File for context"
    :default []
    :assoc-fn (fn [m k v] (update-in m [k] conj v))]
   ["-s" "--sequential-blank-nodes" "Outputs sequential blank nodes instead of random ones. Useful for testing purposes."]
   ["-v" "--version"]
   ["-h" "--help"]])

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn run
  "Given a sequence of command-line argument strings,
   run and exit."
  [args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
      (:help options) (println usage)
      (:version options) (println (version))
      errors (exit 1 (error-msg errors))
      (= 0 (count arguments)) (exit 1 (error-msg ["Specify at least one FILE"]))
      :else
      (let [fmt (or (keyword (:to options))
                    (keyword (:write options))
                    (api/path-format (:output options)))
            context (when (first (:context options))
                      (->> options
                           :context
                           (api/read-paths nil nil)
                           last
                           st/make-context))
            in-raw (api/read-paths
                    (or (:from options) (:read options))
                    context
                    arguments)
            in (if (:sequential-blank-nodes options)
                 (st/sequential-blank-nodes in-raw)
                 in-raw)
            out (if (:output options)
                  (io/file (:output options))
                  System/out)]
        (api/render-file fmt context in out)))))

(defn -main [& args]
  (run args))
