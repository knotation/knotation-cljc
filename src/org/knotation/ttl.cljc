(ns org.knotation.ttl
  (:require [clojure.string :as string]

            [org.knotation.util :as util]
            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]
            [org.knotation.format :as fmt]))

(defn output
  "Given a state and a vector of strings,
   update the state with an output map
   and current :line-number and :column-number."
  [{:keys [line-number column-number]
    :or {line-number 1 column-number 1}
    :as state}
   parse]
  (let [content (->> parse flatten (filter string?) string/join)
        lines (util/split-lines content)]
    (assoc
     state
     :line-number (-> lines count dec (+ line-number))
     :column-number
     (if (second lines)
       (-> lines last count inc)
       (-> lines first count (+ column-number)))
     ::st/output
     #::st{:format :ttl
           :content content
           :line-number line-number
           :column-number column-number})))

(defn render-iri
  "Given an environment and an IRI string,
   return a CURIE or a wrapped IRI string."
  [env iri]
  (or (en/iri->curie env iri)
      (en/iri->wrapped-iri iri)))

(defn render-lexical
  [ol]
  (if (re-find #"\n" ol)
    (str "\"\"\"" ol "\"\"\"")
    (str "\"" ol "\"")))

(defn render-object
  "Given an environment, a sequence of states, and an object node,
   return a (possibly nested) sequence of strings representing the object,
   including nested lists and anonymous subjects."
  [env {::rdf/keys [oi ob ol di lt] :as quad}]
  (cond
    oi (render-iri env oi)

    ob ob

    (and di (not (contains? #{(rdf/xsd "string") (rdf/kn "link")} di)))
    (str (render-lexical ol) "^^" (render-iri env di))

    lt (str (render-lexical ol) "@" lt)

    ol (render-lexical ol)))

(defn render-prefix
  [{:keys [prefix iri] :as state}]
  (output state ["@prefix " prefix ": <" iri "> ." "\n"]))

(defn render-base
  [{:keys [base] :as state}]
  (output state ["@base <" base "> ." "\n"]))

(defn render-stanza-start
  [{:keys [::en/env subject] :or {env {}} :as state}]
  (output state [(if (rdf/blank? subject) subject (render-iri env subject)) "\n"]))

(defn render-stanza-end
  [state]
  (output state ["\n"]))

(defn render-subject-start
  [{:keys [subject list-item terminal] :as state}]
  (output state [(cond list-item "(" (rdf/blank? subject) "[" :else "") terminal]))

(defn render-subject-end
  [{:keys [subject depth list-item terminal] :as state}]
  (output state [(repeat depth "  ") (cond list-item ")" (rdf/blank? subject) "]" :else "") terminal]))

(defn render-statement
  [{:keys [::en/env ::rdf/quad :silent :predicate :object :depth :initial :terminal]
    :or {env {} depth 1}
    :as state}]
  (if silent
    state
    (output
     state
     (concat
      (repeat depth "  ")
      [initial]
      (if predicate
        [predicate]
        [(render-iri env (::rdf/pi quad)) " "])
      (if object
        [object]
        [(render-object env quad)])
      [terminal]))))

(defn render-state
  [{:keys [::st/event] :as state}]
  (case event
    ::st/prefix (render-prefix state)
    ::st/base (render-base state)
    ::st/stanza-start (render-stanza-start state)
    ::st/stanza-end (render-stanza-end state)
    ::st/subject-start (render-subject-start state)
    ::st/subject-end (render-subject-end state)
    ::st/statement (render-statement state)
    state))

(def default-state
  {::en/env {}
   :line-number 1
   :column-number 1})

(defn get-subject
  [state]
  (or
   (get-in state [::rdf/quad ::rdf/si])
   (get-in state [::rdf/quad ::rdf/sb])))

(defn inner-sort-statements
  "Given a map from subjects to sequences of their states,
   plus :subjects and :states sequences,
   and a :depth integer,
   recursively loop through the :subjects and add to :states,
   in the order and depth that Turtle expects."
  [{:keys [subjects depth] :as coll}]
  (if-let [subject (first subjects)]
    (if-let [state (first (get coll subject))]
      (let [coll (update coll subject rest) ; remove this state
            state (assoc state :depth depth)
            ob (-> state ::rdf/quad ::rdf/ob)]
        (if (and ob (find coll ob))
          ; object is nested
          (-> coll
              (update :states conj state)
              (update :states conj {::st/event ::st/subject-start :depth depth :subject ob})
              (update :depth inc)
              ; bump the nested object to the top of the subjects list
              (assoc :subjects (concat [ob] (remove #{ob} subjects)))
              inner-sort-statements)

          ; object is not nested
          (-> coll
              (update :states conj state)
              inner-sort-statements)))

      ; no more states for this subject
      (-> coll
          (update :states conj {::st/event ::st/subject-end :depth (dec depth) :subject subject})
          (dissoc subject)
          (update :subjects rest)
          (update :depth dec)
          inner-sort-statements))

    ; no more subjects
    coll))

(defn sort-statements
  [grouped-states subjects]
  (-> grouped-states
      (assoc :states [] :subjects subjects :depth 1)
      inner-sort-statements
      :states
      (concat [{::st/event ::st/subject-start :depth 1 :subject (first subjects)}])))

(defn flatten-lists
  [states]
  states)

(defn fix-terminals
  [states]
  (map
   (fn [current-state next-state]
     (let [current-event (::st/event current-state)
           next-event  (::st/event next-state)]
       (cond
         (nil? next-state)
         (assoc current-state :terminal " .\n")
         (and (= ::st/statement current-event) (= ::st/subject-start next-event))
         (assoc current-state :object "")
         (and (= ::st/subject-end current-event) (= ::st/statement next-event))
         (assoc current-state :terminal " ;\n")
         (= ::st/statement current-event)
         (assoc current-state :terminal " ;\n")
         (= ::st/subject-start current-event)
         (assoc current-state :terminal "\n")
         (= ::st/subject-end current-event)
         (assoc current-state :terminal "\n")
         :else
         current-state)))
   states
   (concat (rest states) [nil])))

(defn sort-stanza
  "Given a sequence of states for a single stanza,
   reorder and annotate them as required."
  [states]
  (let [statements (-> states rest butlast)
        zn (-> states first :subject)
        grouped (group-by get-subject statements)]
    (concat
     (get grouped nil)
     [(first states)]
     (->> statements
          (map get-subject)
          distinct
          (remove #{zn})
          (concat [zn])
          (remove nil?)
          (sort-statements (dissoc grouped nil))
          flatten-lists
          fix-terminals)
     [(last states)])))

(defn render-stanza
  "Given an environment and a sequence of states for a single stanza,
   return a sequence of states with rendered :output."
  [previous-states states]
  (->> states
       sort-stanza
       (reductions
        (fn [previous-state state]
          (->> state
               (st/update-state previous-state)
               render-state))
        (or (last previous-states) default-state))
       rest))

(defn render-states
  [env states]
  (->> states
       st/partition-stanzas
       (reductions
        (fn [previous-stanza stanza]
          (render-stanza previous-stanza stanza))
        [])
       (mapcat identity)))

(defmethod fmt/render-states
  :ttl
  [fmt env states]
  (render-states env states))
