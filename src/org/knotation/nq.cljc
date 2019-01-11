(ns org.knotation.nq
  (:require [clojure.string :as string]

            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en :refer [wrap-iri]]
            [org.knotation.state :as st]))

(defn render-lexical
  "Given a lexical, escape any special characters. 
   Surround in quotes."
  [ol]
  (str
    "\""
    (string/escape ol char-escape-string)
    "\""))

(defn render-quad
  [{::rdf/keys [gi si sb pi oi ob ol di lt] :as quad}]
  (let [di (when-not (= (rdf/xsd "string") di) di)]
    (->> [(or sb (and si (wrap-iri si)))
          (wrap-iri pi)
          (or ob
              (and oi (wrap-iri oi))
              (and ol lt (str (render-lexical ol) "@" lt))
              (and ol di (str (render-lexical ol) "^^" (wrap-iri di)))
              (and ol (render-lexical ol)))
          (and gi (wrap-iri gi))]
         (remove nil?)
         (interpose " ")
         vec
         (#(conj % " .\n"))
         (apply str))))

(defn render-state
  "Given a state, if it has a quad then add the rendered quad to the state.
   Return the state."
  [{:keys [::rdf/quad] :as state}]
  (if quad
    (st/output state :nq (render-quad quad))
    state))

(defn render-states
  "Given a sequence of states,
   return a lazy sequence of rendered states."
  [states]
  (map render-state states))
