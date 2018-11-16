(ns org.knotation.ttl
  (:require [clojure.string :as string]

            [org.knotation.util :as util]
            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]))

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
  [env {::rdf/keys [pi oi ob ol di lt] :as quad}]
  (let [di (or di (en/get-datatype env pi))
        lt (or lt (en/get-language env pi))]
    (cond
      oi (render-iri env oi)

      ob ob

      (and di (not (contains? #{(rdf/xsd "string") (rdf/kn "link")} di)))
      (str (render-lexical ol) "^^" (render-iri env di))

      lt (str (render-lexical ol) "@" lt)

      ol (render-lexical ol))))

(defn render-blank
  [state]
  "\n")

(defn render-prefix
  [{:keys [::en/prefix ::en/iri] :as state}]
  (str "@prefix " prefix ": <" iri "> .\n"))

(defn render-base
  [{:keys [::en/base] :as state}]
  (str "@base <" base "> .\n"))

(defn render-stanza-start
  [{:keys [::en/env ::rdf/subject] :or {env {}} :as state}]
  nil)

(defn render-stanza-end
  [state]
  nil)

(defn render-subject-start
  [{:keys [::en/env ::rdf/subject ::depth ::list-item ::terminal] :or {env {}} :as state}]
  (str
   (cond
     list-item "("
     (and (rdf/blank? subject) (= depth 0)) subject
     (rdf/blank? subject) "["
     :else (render-iri env subject))
   terminal))

(defn render-subject-end
  [{:keys [::rdf/subject ::depth ::list-item ::terminal] :or {depth 0} :as state}]
  (when (> depth 0)
    (apply
     str
     (concat
      (repeat depth "  ")
      [(cond
         list-item ")"
         (rdf/blank? subject) "]"
         :else "")
       terminal]))))

(defn render-statement
  [{:keys [::en/env ::rdf/quad ::silent ::predicate ::object ::depth ::initial ::terminal]
    :or {env {} depth 1}
    :as state}]
  (when-not silent
    (apply
     str
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
  "Given a state, return a state with optional ::st/output."
  [{:keys [::st/event] :as state}]
  (st/output
   state
   :ttl
   (case event
     ::st/blank (render-blank state)
     ::st/prefix (render-prefix state)
     ::st/base (render-base state)
     ::st/stanza-start (render-stanza-start state)
     ::st/stanza-end (render-stanza-end state)
     ::st/subject-start (render-subject-start state)
     ::st/subject-end (render-subject-end state)
     ::st/statement (render-statement state))))

(declare inner-sort-statements)

(defn annotate-nested
  "When the object of a statement is a blank node
   that points to a branch of this stanza,
   annotate the statement
   then recurse to that branch."
  [{:keys [::subjects ::lists ::list-items ::depth] :as coll} subject state ob]
  ; bump this subject to the top of the list of subjects
  (let [coll (assoc coll ::subjects (concat [ob] (remove #{ob} subjects)))]
    (cond
      ; statement is a "first" list item with nested object
      (-> state ::rdf/quad ::rdf/pi (= (rdf/rdf "first")))
      (-> coll
          (update ::states conj (assoc state ::predicate "" ::object ""))
          (update ::states conj {::st/event ::st/subject-start
                                 ::rdf/subject ob
                                 ::list-item (boolean (find lists ob))
                                 ::depth depth})
          (update ::depth inc)
          inner-sort-statements)

      ; statement is a "rest" list item: do not indent!
      (-> state ::rdf/quad ::rdf/pi (= (rdf/rdf "rest")))
      (-> coll
          (update ::states conj (assoc state ::silent true))
          inner-sort-statements)

      :else
      (-> coll
          (update ::states conj (assoc state ::object ""))
          (update ::states conj {::st/event ::st/subject-start
                                 ::rdf/subject ob
                                 ::list-item (boolean (find lists ob))
                                 ::depth depth})
          (update ::depth inc)
          inner-sort-statements))))

(defn annotate-state
  "Annotate a single state by marking it as nested, last, or a list item,
   then recurse to the next state."
  [{:keys [::subjects ::annotations ::depth] :as coll} subject state]
  (let [last? (= state (last (get coll subject)))
        coll (update coll subject rest) ; remove this state
        state (merge state {::depth depth} (when last? {::last true}))
        ob (-> state ::rdf/quad ::rdf/ob)]
    (if (and ob (find coll ob) (not (contains? annotations ob)))
      (annotate-nested coll subject state ob)
      ; object is not nested
      (let [state
            (cond
              (-> state ::rdf/quad ::rdf/pi (= (rdf/rdf "first")))
              (assoc state ::predicate "")

              (-> state ::rdf/quad ::rdf/pi (= (rdf/rdf "rest")))
              (assoc state ::silent true)

              :else
              state)]
        (-> coll
            (update ::states conj state)
            inner-sort-statements)))))

(defn annotate-subject
  "Annotate a subject, either by annotating its next remaining state,
   or annotating it as complete and recursing to the next subject."
  [{:keys [::subjects ::lists ::list-items ::depth] :as coll} subject]
  (if-let [state (first (get coll subject))]
    (annotate-state coll subject state)

    ; no more states for this subject
    (let [coll (-> coll
                   (dissoc subject)
                   (assoc ::subjects (remove #{subject} subjects)))
          subject-end {::st/event ::st/subject-end
                       ::list-item (boolean (find lists subject))
                       ::depth (dec depth)
                       ::rdf/subject subject}
          next-subject (second subjects)]
      (cond
        ; There's another top-level subject to handle
        (and next-subject (= depth 1))
        (-> coll
            (update ::states conj subject-end)
            (update ::states conj {::st/event ::st/blank ::depth 0})
            (update ::states conj {::st/event ::st/subject-start
                                   ::rdf/subject next-subject
                                   ::depth 0})
            inner-sort-statements)

        ; subject is an item inside a list: do not unindent!
        (and (contains? list-items subject) (not (find lists subject)))
        (inner-sort-statements coll)

        :else
        (-> coll
            (update ::states conj subject-end)
            (update ::depth dec)
            inner-sort-statements)))))

(defn inner-sort-statements
  "Given a map from subjects to sequences of their states,
   plus :subjects and ::states sequences,
   a :lists map and a ::depth integer,
   recursively loop through the :subjects and add to :states,
   in the order and depth that Turtle expects."
  [{:keys [::subjects] :as coll}]
  (if-let [subject (first subjects)]
    (annotate-subject coll subject)
    coll))

(defn sort-statements
  [grouped-states lists annotations subjects]
  (concat
   (when-let [s (first subjects)]
     [{::st/event ::st/subject-start ::rdf/stanza s ::rdf/subject s ::depth 0}])
   (-> grouped-states
       (assoc ::states []
              ::subjects subjects
              ::lists lists
              ::list-items (-> lists vals flatten set)
              ::annotations annotations
              ::depth 1)
       inner-sort-statements
       ::states)))

(defn annotate-terminal
  "Given a state, add the right :terminal annotation."
  [{:keys [::st/event ::depth ::last ::object] :as state}]
  (cond
    object
    state

    (= ::st/statement event)
    (cond
      last
      (if (= 1 depth)
        (assoc state ::terminal " .\n")
        (assoc state ::terminal "\n"))

      (-> state ::rdf/quad ::rdf/pi (= (rdf/rdf "first")))
      (assoc state ::terminal "\n")

      :else
      (assoc state ::terminal " ;\n"))

    (= ::st/subject-start event)
    (assoc state ::terminal "\n")

    (= ::st/subject-end event)
    (if (= 1 depth)
      (assoc state ::terminal " ;\n")
      (assoc state ::terminal "\n"))

    :else
    state))

(defn sort-stanza
  "Given a sequence of states for a single stanza,
   reorder and annotate them as required."
  [states]
  (let [zn (-> states first ::rdf/quad ::rdf/zn)
        grouped (group-by st/get-subject states)
        lists (->> states (map ::rdf/quad) rdf/collect-lists)
        annotations (->> states
                         (map ::rdf/quad)
                         (filter #(= (rdf/owl "annotatedSource") (::rdf/pi %)))
                         (map ::rdf/sb)
                         set)]
    (->> states
         (map st/get-subject)
         distinct
         (remove #{zn})
         (concat [zn])
         (remove nil?)
         (sort-statements (dissoc grouped nil) lists annotations)
         (map annotate-terminal)
         (concat (get grouped nil)))))

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
        (or (last previous-states) st/default-state))
       rest))

(defn render-states
  [env states]
  (->> states
       (filter #(contains? #{::st/prefix ::st/base ::st/statement} (::st/event %)))
       (partition-by ::rdf/stanza)
       (interpose [{::st/event ::st/blank}])
       (reductions
        (fn [previous-stanza stanza]
          (render-stanza previous-stanza stanza))
        [])
       (mapcat identity)))
