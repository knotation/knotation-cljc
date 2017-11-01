(ns org.knotation.api
  (:require [org.knotation.kn :as kn]
            [org.knotation.nq :as nq]))

(defn input->states
  [{:keys [format] :as input}]
  (case format
    :kn (kn/processor input)
    (throw (Exception. (str "Unknown format: " format " for input " input)))))

(defn inputs->states
  [inputs]
  ((->> inputs
        (map input->states)
        reverse
        (apply comp))
   []))

(defn process-pipeline
  [{:keys [inputs outputs] :as pipeline}]
  (->> inputs
       inputs->states
       nq/states->lines))

(defn kn
  [& contents]
  (->> contents
       (map
        (fn [c]
          {:format :kn
           :lines (clojure.string/split-lines c)}))
       inputs->states))

(defn env
  [states]
  (->> states last :env))

(def example-kn
  "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
@prefix ex: <https://example.com/>

: ex:foo
rdfs:label: Foo
ex:comment: comment")
