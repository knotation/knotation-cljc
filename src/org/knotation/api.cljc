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
