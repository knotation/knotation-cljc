(ns org.knotation.state
  (:require [clojure.string :as string]
            [org.knotation.util :as util]
            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]))

(def error-messages
  {:not-a-comment "Not a comment line"
   :not-a-prefix-line "Not a @prefix line"
   :not-a-subject-line "Not a subject line"
   :not-a-statement "Not a statement"
   :unrecognized-predicate "Unrecognized predicate:"
   :unrecognized-datatype "Unrecognized datatype:"})

(defn error
  [state error-type & info]
  (->> info
       (map str)
       (concat [(get error-messages error-type "ERROR:")])
       (string/join " ")
       (assoc
        {::error-type error-type}
        ::error-message)
       (merge (when info {::error-info info}))
       (assoc state ::event ::error ::error)))

(def default-location
  {::line-number 1
   ::column-number 1})

(defn step-location
  "Given a start location, step forward one column,
   and return the new location."
  [{:keys [::line-number ::column-number] :as start}]
  (update start ::column-number inc))

(defn advance-location
  "Given a start location and a content string,
   return an end location."
  [{:keys [::line-number ::column-number] :as start} content]
  (let [lines (util/split-lines content)]
    {::line-number (-> lines count dec (+ line-number))
     ::column-number
     (if (second lines)
       (-> lines last count)
       (-> lines first count dec (+ column-number)))}))

(defn input
  "Given a state, a format keyword, and an input string (or nil)
   update the state with an ::input map and current ::location."
  [{:keys [::location] :or {location default-location} :as state} format content]
  (if (and content (string? content))
    (let [end (advance-location location content)]
      (assoc
       state
       ::input
       {::format format
        ::content content
        ::start location
        ::end end}))
    state))

(defn output
  "Given a state, a format keyword, and an output string (or nil)
   update the state with an ::output map and current ::location."
  [{:keys [::location] :or {location default-location} :as state} format content]
  (if (and content (string? content))
    (let [end (advance-location location content)]
      (assoc
       state
       ::location (step-location end)
       ::output
       {::format format
        ::content content
        ::start location
        ::end end}))
    state))

(defn render-output
  "Given a sequence of states with ::output,
   return a sequence of string contents."
  [states]
  (->> states
       (map ::output)
       (map ::content)
       (filter string?)))

(defn render-output-string
  "Given a sequence of states with ::output,
   return a string."
  [states]
  (->> states
       render-output
       string/join))

(defn update-env
  "Given an environment and a state,
   return an updated environment."
  [env {:keys [prefix iri ::rdf/quad] :as state}]
  (let [{::rdf/keys [si pi oi ol]} quad]
    (cond
      (and prefix iri)
      (en/add-prefix env prefix iri)

      ; TODO: make this configurable
      ; WARN: case macro requires literal values, not symbols or functions
      (::rdf/pi quad)
      (case (::rdf/pi quad)
        "http://www.w3.org/2000/01/rdf-schema#label"
        (en/add-label env ol si)

        "https://knotation.org/kn/default-datatype"
        (en/set-datatype env si oi)

        "https://knotation.org/kn/default-language"
        (en/set-language env si ol)

        "https://knotation.org/kn/template-content"
        (en/set-template-content env si ol)

        ;else
        env)

      :else
      env)))

(defn update-state
  "Given a previous state and the current state,
   use the previous state to assign an environment to the current state."
  [{:keys [::en/env ::location] :or {env {} location default-location} :as previous-state} state]
  (merge
   state
   (when-let [subject (or (:subject state) (:subject previous-state))]
     {:subject subject})
   {::en/env (update-env env previous-state)
    ::location location}))

(defn sequential-blank-nodes
  "Given a sequence of states, some of which have ::rdf/quads,
   return a lazy sequence of states with sequential blank nodes."
  [states]
  (->> states
       (reductions
        (fn [[coll _] {:keys [::rdf/quad :subject] :as state}]
          (cond
            quad
            (let [[coll sb] (rdf/replace-blank-node coll (::rdf/sb quad))
                  [coll ob] (rdf/replace-blank-node coll (::rdf/ob quad))
                  [coll zn] (rdf/replace-blank-node coll (when (rdf/blank? (::rdf/zn quad))
                                                           (::rdf/zn quad)))]
              [coll
               (assoc
                state
                ::rdf/quad
                (merge quad
                       (when zn {::rdf/zn zn})
                       (when sb {::rdf/sb sb})
                       (when ob {::rdf/ob ob})))])

            subject
            (let [[coll zn] (rdf/replace-blank-node coll (when (rdf/blank? subject) subject))]
              [coll
               (if zn
                 (assoc state :subject zn)
                 state)])

            :else
            [coll state]))
        [{::rdf/counter 0} nil])
       rest
       (map second)))

(defn objects-subjects
  [states]
  (->> states
       (map ::rdf/quad)
       (remove nil?)
       (rdf/objects-subjects)))

(defn subjects-blank-objects
  [states]
  (->> states
       (map ::rdf/quad)
       (remove nil?)
       (rdf/subjects-blank-objects)))

(defn assign-stanza
  [coll {:keys [::rdf/quad] :as state}]
  (if quad
    (assoc state ::rdf/quad (rdf/assign-stanza coll quad))
    state))

(defn assign-stanzas
  "Given a sequence of state maps,
   assume that blank node constructs are consecutive,
   and return a lazy sequence of quad maps with stanza assigned."
  [states]
  (->> states
       (#(concat % [nil]))
       (reductions
        (fn [{:keys [stored stanza] :as coll} state]
          (let [si (-> state ::rdf/quad ::rdf/si)]
            (cond
              (nil? state)
              {:results stored}
              (and si (not= si stanza))
              {:results stored
               :stored [state]
               :stanza si}
              :else
              {:stored (conj stored state)
               :stanza stanza})))
        {:stored [] :stanza nil})
       (map :results)
       (remove nil?)
       (mapcat #(map (partial assign-stanza (objects-subjects %)) %))))

(defn partition-stanzas
  "Given a sequence of states,
   partition into sequences of states in the same stanza
   (or outside any stanza)."
  [states]
  (partition-by
   (fn [state]
     (or (-> state ::rdf/quad ::rdf/zn)
         (-> state :subject)))
   states))

(defn partition-subjects
  "Given a sequence of states,
   partition into sequences of states with the same subject
   (or without a subject)."
  [states]
  (partition-by
   (fn [{:keys [::rdf/quad] :as state}]
     (or (::rdf/si quad) (::rdf/sb quad)))
   states))

(def blank-state
  {::event ::blank
   ::en/env {}})

(def default-state
  (assoc blank-state ::en/env en/default-env))
