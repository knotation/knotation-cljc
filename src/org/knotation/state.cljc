(ns org.knotation.state)

(defn add-prefix
  [state prefix iri]
  (-> state
      (assoc-in [:env :prefix-iri prefix] iri)
      (assoc-in [:env :iri-prefix iri] prefix)))

(defn add-label
  [state label iri]
  (-> state
      (assoc-in [:env :label-iri label] iri)
      (assoc-in [:env :iri-label iri] label)))

(defn update-state
  [state {:keys [subject predicate object] :as quad}]
  ; TODO: make this configurable
  (case (:iri predicate)
    "http://www.w3.org/2000/01/rdf-schema#label"
    (add-label state (:lexical object) (:iri subject))
    state))

(defn update-location
  [state line]
  (-> state
      (update :line-number inc)
      (assoc :line line)))
