(ns org.knotation.state
  (:require [clojure.string :as string]
            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]))

;; # Defaults

(def default-location
  {::line-number 1
   ::column-number 1})

(def default-state
  {::event ::default})

;; # Errors

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

;; # Locations

(defn step-location
  "Given a start location, step forward one column,
   and return the new location."
  [{:keys [::line-number ::column-number] :as start}]
  (update start ::column-number inc))

(defn advance-location
  "Given a start location and a content string,
   return an end location."
  [{:keys [::line-number ::column-number] :as start} content]
  (let [lines (string/split content #"\n" -1)] ; NOTE: NOT util/split-lines
    {::line-number (-> lines count dec (+ line-number))
     ::column-number
     (if (second lines)
       (-> lines last count)
       (-> lines first count dec (+ column-number)))}))

;; # Input and Output

(defn input
  "Given a state, a format keyword, and an input string (or nil)
   update the state with an ::input map and current ::location."
  [{:keys [::location] :or {location default-location} :as state} format content]
  (if (and content (string? content))
    (let [end (advance-location location content)]
      (assoc
       state
       ::event ::input
       ::location (step-location end)
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

(defn render-parse
  "Given a parse, return a string or nil."
  [parse]
  (->> parse
       flatten
       (filter string?)
       (#(when (first %) (string/join %)))))

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

;; # Updating State

(defn update-env
  "Given an environment and a state,
   return an updated environment."
  [env {:keys [::en/prefix ::en/iri ::en/base ::rdf/quad] :as state}]
  (let [{::rdf/keys [si pi oi ol]} quad]
    (cond
      (and prefix iri)
      (en/add-prefix env prefix iri)

      base
      (en/add-base env base)

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

(defn strip-state
  "Given a state, dissoc certain keys based on the event type."
  [{:keys [::event] :as state}]
  (case event
    (::prefix ::base) (dissoc state ::rdf/graph ::rdf/stanza ::rdf/subject)
    (::graph-start ::graph-end) (dissoc state ::rdf/stanza ::rdf/subject)
    (::stanza-start ::stanza-end) (dissoc state ::rdf/subject)
    state))

(defn update-state
  "Given a previous state and the current state,
   use the previous state to assign an environment to the current state."
  [{:keys [::en/env ::location ::next-subject ::quad-stack] :or {env en/default-env} :as previous-state}
   {:keys [::event ::rdf/quad] :as state}]
  (strip-state
   (merge
    state
    (let [env (update-env env previous-state)]
      (when (and env (not= env en/default-env))
        {::en/env env}))
    (when (and (::location previous-state) (not (::location state)))
      {::location location})
    (when quad-stack
      {::quad-stack quad-stack})
    (when-let [g (or (::rdf/gi quad) (::rdf/graph state) (::rdf/graph previous-state))]
      {::rdf/graph g})
    (when-let [s (or (::rdf/zn quad) (::rdf/stanza state) (::rdf/stanza previous-state))]
      {::rdf/stanza s})
    (when-let [s (or (::rdf/si quad) (::rdf/sb quad) (::rdf/subject state) (::rdf/subject previous-state))]
      {::rdf/subject s})
    ; override subject
    (when next-subject
      {::rdf/subject next-subject}))))

(defn update-states
  [previous-state states]
  (->> states
       (reductions update-state previous-state)
       rest))

;; # Quads inside States

;; These are mostly wrappers around functions in the RDF namepace.
;; TODO: Use specter to simplify?
(defn get-subject
  [state]
  (or
   (get-in state [::rdf/quad ::rdf/si])
   (get-in state [::rdf/quad ::rdf/sb])))

(defn sequential-blank-nodes
  "Given a sequence of states, some of which have ::rdf/quads,
   return a lazy sequence of states with sequential blank nodes."
  [states]
  (->> states
       (reductions
        (fn [[coll _] {:keys [::rdf/quad ::rdf/stanza ::rdf/subject] :as state}]
          (if quad
            (let [[coll sb] (rdf/replace-blank-node coll (::rdf/sb quad))
                  [coll ob] (rdf/replace-blank-node coll (::rdf/ob quad))
                  [coll zn] (rdf/replace-blank-node coll (when (and (::rdf/zn quad) (rdf/blank? (::rdf/zn quad)))
                                                           (::rdf/zn quad)))]
              [coll
               (assoc
                state
                ::rdf/stanza (or zn (::rdf/zn quad))
                ::rdf/subject (or (::rdf/si quad) sb)
                ::rdf/quad
                (merge quad
                       (when zn {::rdf/zn zn})
                       (when sb {::rdf/sb sb})
                       (when ob {::rdf/ob ob})))])
            (let [[coll zn] (rdf/replace-blank-node coll (when (and stanza (rdf/blank? stanza)) stanza))
                  [coll sb] (rdf/replace-blank-node coll (when (and subject (rdf/blank? subject)) subject))]
              [coll
               (merge
                state
                (when-let [zn (or zn stanza)] {::rdf/stanza zn})
                (when-let [sn (or sb subject)] {::rdf/subject sn}))])))
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
   and return a lazy sequence of states with stanza assigned to the quad."
  [states]
  (->> (concat states [nil])
       (reductions
        (fn [{:keys [stored stanza] :as coll} state]
          (let [si (-> state ::rdf/quad ::rdf/si)]
            (cond
              (nil? state)
              {:results stored}
              (and si stanza (not= si stanza))
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

;; # Insert Events

; TODO: insert-graph-events

(defn insert-stanza-separators
  [states]
  (->> states
       (partition-by ::rdf/stanza)
       (mapcat
        (fn [states]
          (concat
           states
           [(update-state
             (last states)
             (-> states
                 last
                 (select-keys [::location ::rdf/stanza ::rdf/subject])
                 (assoc ::event ::blank)))])))
       butlast))

(defn insert-stanza-events
  "Given a sequence of states, add ::stanza-start and ::stanza-end events as required."
  [states]
  (->> states
       (partition-by ::rdf/stanza)
       (mapcat
        (fn [states]
          (concat
           (when (-> states first ::rdf/stanza)
             [(-> states
                  first
                  (select-keys [::en/env ::location ::rdf/stanza])
                  (assoc ::event ::stanza-start))])
           (remove #(contains? #{::stanza-start ::stanza-end} (::event %)) states)
           (when (-> states first ::rdf/stanza)
             [(update-state
               (last states)
               (-> states
                   last
                   (select-keys [::location ::rdf/stanza])
                   (assoc ::event ::stanza-end)))]))))))

(defn insert-subject-events
  "Given a sequence of states, add ::stanza-start and ::stanza-end events as required."
  [states]
  (->> states
       (partition-by ::rdf/subject)
       (mapcat
        (fn [states]
          (concat
           (when (-> states first ::rdf/subject)
             [(-> states
                  first
                  (select-keys [::en/env ::location ::rdf/stanza ::rdf/subject])
                  (assoc ::event ::subject-start))])
           (remove #(contains? #{::subject-start ::subject-end} (::event %)) states)
           (when (-> states first ::rdf/subject)
             [(update-state
               (last states)
               (-> states
                   last
                   (select-keys [::location ::rdf/stanza ::rdf/subject])
                   (assoc ::event ::subject-end)))]))))))

(defn insert-events
  "Given a sequence of states, add start and end events."
  [states]
  (->> states
       insert-subject-events
       insert-stanza-events
       insert-stanza-separators))
