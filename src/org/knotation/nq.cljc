(ns org.knotation.nq
  (:require [clojure.string :as string]

            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en :refer [wrap-iri]]
            [org.knotation.state :as st]))

(defn render-quad
  [{::rdf/keys [gi si sb pi oi ob ol di lt] :as quad}]
  (->> [(or sb (and si (wrap-iri si)))
        (wrap-iri pi)
        (or ob
            (and oi (wrap-iri oi))
            (and ol lt (str "\"" ol "\"@" lt))
            (and ol di (str "\"" ol "\"^^" (wrap-iri di)))
            (and ol (str "\"" ol "\"")))
        (and gi (wrap-iri gi))]
       (remove nil?)
       (interpose " ")
       vec
       (#(conj % " .\n"))
       (apply str)))

(defn render-state
  "Given a state, if it has a quad then add the rendered quad to the state.
   Return the state."
  [{:keys [::rdf/quad] :as state}]
  (if quad
    (->> quad
         render-quad
         (assoc {::st/format :nq} ::st/content)
         (assoc state ::st/output))
    state))

(defn render-states
  "Given a sequence of states,
   return a lazy sequence of rendered states."
  [states]
  (map render-state states))
