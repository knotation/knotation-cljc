(ns org.knotation.ttl
  (:require [clojure.string :as string]
            [clojure.zip :as zip]

            [org.knotation.util :as util]
            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]))

(defn render-iri
  "Given an environment and an IRI string,
   return a CURIE or a wrapped IRI string."
  [env iri]
  (let [curie (en/iri->curie env iri)]
    (if (and curie (not (re-find #"/" curie)))
      curie
      (en/iri->wrapped-iri iri))))

(defn render-lexical
  "Given a literal, surround it with quotes to render it. 
   If there is a newline or a double quote character, 
   triple-quote the literal."
  [ol]
  (let [s (string/escape ol {\" "\\\""})]
    (if (re-find #"\n" ol)
      (str "\"\"\"" s "\"\"\"")
      (str "\"" s "\""))))

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
  [{:keys [::en/env ::rdf/subject ::lexical ::terminal] :or {env {}} :as state}]
  (str (or lexical
           (when (rdf/blank? subject) subject)
           (render-iri env subject))
       terminal))

(defn render-subject-end
  [{:keys [::rdf/subject ::depth ::lexical ::terminal] :or {depth 0} :as state}]
  (when (and lexical terminal)
    (apply
     str
     (concat
      (repeat (dec depth) "  ")
      [lexical terminal]))))

(defn render-statement
  [{:keys [::en/env ::rdf/quad ::predicate ::object ::depth ::initial ::terminal]
    :or {env {} depth 1}
    :as state}]
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
      [(render-object env quad)
       terminal]))))

(defn render-state
  "Given a state, return a state with optional ::st/output."
  [{:keys [::st/event ::silent] :as state}]
  (if silent
    state
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
       ::st/statement (render-statement state)
       (util/throw-exception "Bad event" event "in state" state)))))

;; # Tree Representation
;;
;; Process the Turltle nodes as a tree using clojure.zip.
;; First we create a zipper using a vector of ::st/children states.
;; Then we build the tree by inserting nested children.
;; Then we flatten the tree with a depth-first traversal,
;; inserting subject-start and subject-end states,
;; handling anonymous nodes,
;; and keeping RDF list elements at the same depth.

(defn state-tree
  "Given a state, return a zipper using ::st/children."
  [state]
  (zip/zipper
   (constantly true)
   ::st/children
   (fn [node children] (assoc node ::st/children children))
   state))

(defn annotate-statement
  "Given a map from subjects to their states,
   with a current subject and state,
   return that state ready to render."
  [{:keys [::subjects] :as coll}]
  (let [subject (first subjects)
        state (first (get coll subject))
        pi (-> state ::rdf/quad ::rdf/pi)
        state (cond
                (= (rdf/rdf "first") pi)
                (assoc state ::predicate "" ::terminal "\n")
                (= state (last (get coll subject)))
                (if (-> coll ::tree zip/path count (> 2))
                  (assoc state ::terminal "\n")
                  (assoc state ::terminal " .\n"))
                :else
                (assoc state ::terminal " ;\n"))]
    (assoc state ::silent (= (rdf/rdf "rest") pi))))

(defn annotate-subject-start
  "Given a map from subjects to their states,
   with a current subject and state,
   return a ::st/subject-start state ready to render."
  [{:keys [::subjects ::tree] :as coll}]
  (let [subject (first subjects)
        state (first (get coll subject))
        quad (::rdf/quad state)
        pi (::rdf/pi quad)
        parent (zip/node tree)
        list-item? (contains? #{(rdf/rdf "first") (rdf/rdf "rest")} pi)
        list-head? (and parent (->> parent ::rdf/quad ::rdf/pi (not= (rdf/rdf "rest"))))]
    {::st/event ::st/subject-start
     ::rdf/stanza (::rdf/zn quad)
     ::rdf/subject subject
     ::silent (and list-item? (not list-head?))
     ::lexical (when (-> tree zip/path count (> 1))
                 (if list-item? "(" "["))
     ::terminal "\n"}))

(defn annotate-subject-end
  "Given a map from subjects to their states,
   with a current subject and state,
   return a ::st/subject-end state ready to render."
  [{:keys [::subjects ::tree] :as coll}]
  (let [subject (first subjects)
        state (-> tree zip/down zip/rightmost zip/node)
        quad (::rdf/quad state)
        pi (::rdf/pi quad)
        parent (zip/node tree)
        list-item? (contains? #{(rdf/rdf "first") (rdf/rdf "rest")} pi)
        list-end? (and (= (rdf/rdf "rest") pi) (= (rdf/rdf "nil") (::rdf/oi quad)))]
    {::st/event ::st/subject-end
     ::rdf/stanza (::rdf/zn quad)
     ::rdf/subject subject
     ::silent (and list-item? (not list-end?))
     ::lexical (when (-> tree zip/path count (> 1))
                 (if list-item? ")" "]"))
      ; copy the terminal from the first ancestor statement that's not a list
     ::terminal (->> tree
                     zip/path
                     (filter ::rdf/quad)
                     (drop-while #(= (rdf/rdf "rest") (-> % ::rdf/quad ::rdf/pi)))
                     first
                     ::terminal)}))

(defn build-tree
  "Given a map from subjects to their states,
   with a sequence of ::subjects to process,
   a set of ::annotations,
   and a state-tree at ::tree,
   recursively insert states into the tree
   in the order Turtle should render them,
   and return the updated collection."
  [{:keys [::annotations] :as coll}]
  (loop [coll coll]
    (if-let [subject (first (::subjects coll))]
      (recur
       (if-let [state (first (get coll subject))]
         (let [state (annotate-statement coll)
               coll (if (->> coll ::tree zip/node ::rdf/subject (= subject))
                      coll
                      (-> coll
                          (update ::tree zip/append-child (annotate-subject-start coll))
                          (update ::tree zip/down)
                          (update ::tree zip/rightmost)))
               ob (-> state ::rdf/quad ::rdf/ob)]
           (if (and ob (find coll ob) (not (contains? annotations ob)))
             ; descend into a nested subject
             (-> coll
                 (update ::tree zip/append-child (assoc state ::object ""))
                 (update subject rest)
                 (update ::tree zip/down)
                 (update ::tree zip/rightmost)
                 (assoc ::subjects (->> coll ::subjects (remove #{ob}) (concat [ob]))))

             ; just add the state
             (-> coll
                 (update ::tree zip/append-child state)
                 (update subject rest))))

         ; no more states for this subject, ascend the tree
         (-> coll
             (update ::tree zip/append-child (annotate-subject-end coll))
             (update ::tree zip/up)
             (#(if (->> % ::tree zip/up) (update % ::tree zip/up) %))
             (update ::subjects #(remove #{subject} %)))))

      ; no more subjects
      coll)))

(defn flatten-tree
  "Given a state-tree, return a flat sequence of states."
  [state-tree]
  (loop [states []
         loc state-tree]
    (if (zip/end? loc)
      states
      (recur
       (concat
        states
        (when (and (= 1 (count (zip/path loc))) (zip/left loc))
          [{::st/event ::st/blank}])
        [(-> loc
             zip/node
             (dissoc ::st/children)
             (assoc
              ; depth not counting lists
              ::depth
              (->> loc
                   zip/path
                   (map ::rdf/quad)
                   (map ::rdf/pi)
                   (remove nil?)
                   (remove #{(rdf/rdf "rest")})
                   count
                   inc)))])
       (zip/next loc)))))

(defn sort-statements
  "Given a map from subjects to their states,
   a set of annotations,
   and a sequence of subjects,
   return a sequence of states ready to be rendered."
  [grouped-states annotations subjects]
  (-> grouped-states
      (assoc ::tree (state-tree st/default-state)
             ::subjects subjects
             ::annotations annotations)
      build-tree
      ::tree
      zip/root
      state-tree
      flatten-tree
      rest))

(defn sort-stanza
  "Given a sequence of states for a single stanza,
   reorder and annotate them as required."
  [states]
  (let [zn (->> states (map ::rdf/stanza) first)
        grouped (group-by ::rdf/subject states)
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
         (sort-statements (dissoc grouped nil) annotations)
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
  [previous-state states]
  (->> states
       (filter #(contains? #{::st/prefix ::st/base ::st/statement} (::st/event %)))
       (partition-by ::rdf/stanza)
       (interpose [{::st/event ::st/blank}])
       (reductions
        (fn [previous-stanza stanza]
          (render-stanza previous-stanza stanza))
        [previous-state])
       (mapcat identity)))
