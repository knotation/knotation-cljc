(ns org.knotation.state)

(defn add-prefix
  [state prefix iri]
  (-> state
      (assoc-in [:env :prefix-iri prefix] iri)
      (assoc-in [:env :iri-prefix iri] prefix)))
