(ns org.knotation.cli
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.tools.cli :refer [parse-opts]]
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
      (api/render-file
       (or (keyword (:to options))
           (keyword (:write options))
           (api/path-format (:output options)))
       nil
       (api/read-paths
        (or (:from options) (:read options))
        nil
        arguments)
       (if (:output options)
         (io/file (:output options))
         System/out)))))

(defn -main [& args]
  (run args))
