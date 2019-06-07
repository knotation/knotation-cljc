(ns org.knotation.rdf
  (:require [clojure.string :as string]
            [clojure.pprint :as pp]))

; # Namespaces

(def rdf (partial apply str "http://www.w3.org/1999/02/22-rdf-syntax-ns#"))
(def rdfs (partial apply str "http://www.w3.org/2000/01/rdf-schema#"))
(def xsd (partial apply str "http://www.w3.org/2001/XMLSchema#"))
(def owl (partial apply str "http://www.w3.org/2002/07/owl#"))
(def kn (partial apply str "https://knotation.org/kn/"))
(def ex (partial apply str "http://example.com/"))

; # Blank Nodes

(defn update-blank-node
  "Given a seq of states, a blank node ID, and a seq of predicate-object maps, 
   replace any instances of the blank node ID with the seq of maps."
  [states bnode-id pred-objs]
  (reduce
    (fn [updated state]
      (if-let [ob (->> state ::quad ::ob)]
        (if (= ob bnode-id)
          (conj updated (assoc-in state [::quad ::ob] pred-objs))
          (conj updated state))
        (conj updated state)))
    () states))

(defn update-blank-nodes
  "Given a seq of states and a map of blank node IDs to seqs of predicate-object 
   maps, replace all instances of the blank node IDs with the seq of maps."
  [states bnode-map]
  (reduce-kv
    (fn [updated bnode-id trps]
      (update-blank-node updated bnode-id trps))
    states bnode-map))

(defn expand-ob-value
  "Given a map of blank node IDs to seqs of predicate-object maps and one 
   predicate-object map, expand any blank objects in the predicate-object map."
  [bnode-map pred-obj]
  (if-let [ob (::ob pred-obj)]
    (let [pred-objs (reduce 
                      (fn [v po] 
                        (conj v (expand-ob-value bnode-map po)))
                      [] (get bnode-map ob))]
      (assoc pred-obj ::ob pred-objs))
    pred-obj))

(defn expand-ob-values
  "Given a map of blank node IDs to seqs of predicate-object maps,
   expand any blank objects in the seqs."
  [bnode-map]
  (reduce-kv
    (fn [m bnode pred-objs]
      (let [new-pos (reduce
                      (fn [s po]
                        (conj s (expand-ob-value bnode-map po)))
                      [] pred-objs)]
        (assoc m bnode new-pos)))
    {} bnode-map))

(defn expand-blank-nodes
  "Given a seq of states, replace any blank node IDs with a vector of 
   predicate-object maps, supporting nested blank nodes."
  [states]
  (let [quads (map ::quad states)
        blank-objects (->> quads 
                           (map ::ob) 
                           distinct 
                           (remove nil?))
        bnode-map (reduce
                    (fn [m bnode]
                      (let [trps (filter #(= (::sb %) bnode) quads)]
                        (assoc m bnode (vec (map #(select-keys % [::pi ::oi ::ob]) trps)))))
                      {} blank-objects)]
    (->> bnode-map
         expand-ob-values
         (update-blank-nodes states))))

(defn blank?
  [s]
  (and (string? s) (string/starts-with? s "_:")))

(defn random-blank-node
  "Return a random blank node (UUID)."
  []
  (str
   "_:"
   #?(:clj (java.util.UUID/randomUUID)
      :cljs (random-uuid))))

(defn replace-blank-node
  "Given a map from old to new blank-node strings with a ::counter integer,
   and a node string (or nil),
   return the pair of the updated map and the new node."
  [{:keys [::counter] :as coll} node]
  (cond
    (nil? node) [coll node]
    (find coll node) [coll (get coll node)]
    :else
    (let [new-node (str "_:b" counter)]
      [(-> coll (update ::counter inc) (assoc node new-node)) new-node])))

(defn sequential-blank-nodes
  "Given a sequence of maps that might be quads,
   return a lazy sequence of maps with sequential blank nodes."
  [quads]
  (->> quads
       (reductions
        (fn [[coll _] {:keys [::zn ::sb ::ob] :as quad}]
          (let [[coll sb] (replace-blank-node coll sb)
                [coll ob] (replace-blank-node coll ob)
                [coll zn] (replace-blank-node coll (when (blank? zn) zn))]
            [coll
             (merge quad
                    (when zn {::zn zn})
                    (when sb {::sb sb})
                    (when ob {::ob ob}))]))
        [{::counter 0} nil])
       rest
       (map second)))

(defn rdf-anonymous-subject?
  "Given a sequence of quads and a blank node,
   return true if the node is the subject of some triples, false otherwise."
  [quads sb]
  (->> quads
       (filter #(= sb (:sb %)))
       first
       nil?
       not))

; # RDF Lists
;
; An RDF list is a linked list represented as a tree,
; where each node has one rdf:first and one rdf:rest triple,
; and the final node has rdf:rest rdf:nil.

(defn rdf-list?
  "Given a sequence of quads and a head node,
   return true if it is the head of an RDF list, false otherwise."
  [quads head]
  (->> quads
       (filter #(= head (or (::si %) (::sb %))))
       (filter #(= (rdf "first") (::pi %)))
       first
       nil?
       not))

(defn make-list
  "Given a sequence of object maps,
   return a sequence of quads for an RDF list of those objects."
  [objects]
  (loop [quads []
         b1 (random-blank-node)
         objects objects]
    (cond
      (second objects)
      (let [b2 (random-blank-node)]
        (recur
         (concat
          quads
          [(assoc (first objects) ::sb b1 ::pi (rdf "first"))
           {::sb b1 ::pi (rdf "rest") ::ob b2}])
         b2
         (rest objects)))

      (first objects)
      (concat
       quads
       [(assoc (first objects) ::sb b1 ::pi (rdf "first"))
        {::sb b1 ::pi (rdf "rest") ::oi (rdf "nil")}])

      :else
      quads)))

(defn collect-list
  "Given a sequence of quads and the head of a list,
   return a sequence of the list's rdf:first triples."
  [quads head]
  (loop [firsts []
         head head]
    (if (rdf-list? quads head)
      (recur
       (->> quads
            (filter #(= head (or (:si %) (:sb %))))
            (filter #(= (rdf "first") (:pi %)))
            first
            (conj firsts))
       (->> quads
            (filter #(= head (or (:si %) (:sb %))))
            (filter #(= (rdf "rest") (:pi %)))
            (remove #(= (rdf "nil") (:oi %)))
            first
            (#(or (:oi %) (:ob %)))))
      firsts)))

(defn collect-lists
  "Given a sequence of quads,
   return a map from list head (blank node)
   to a sequence of the blank nodes in the list."
  [quads]
  (let [pairs (->> quads
                   (filter #(= (rdf "rest") (::pi %)))
                   (map (juxt ::ob ::sb)))
        tails (->> pairs
                   (filter #(nil? (first %)))
                   (map second))
        coll (dissoc (into {} pairs) nil)]
    ; For each tail, recursively trace back to the head.
    (->> tails
         (map
          #(loop [heads []
                  tail %]
             (if (find coll tail)
               (recur (conj heads tail) (get coll tail))
               [tail (reverse (conj heads tail))])))
         (into {}))))

; # OWL Annotations
;
; OWL annotations use three quads to pick out a triple,
; and then make a statement about it.

(defn annotation-subjects
  "Given a sequence of states,
   return a set of the subjects that represent OWL annotations."
  [quads]
  (->> quads
       (filter #(= (owl "annotatedSource") (::pi %)))
       (map ::sb)
       set))

(defn annotation-target
  "Given the quads for an OWL annotation,
   return the quad that is the target of the annotation."
  [quads]
  (let [source (->> quads (filter #(= (owl "annotatedSource") (::pi %))) first)
        property (->> quads (filter #(= (owl "annotatedProperty") (::pi %))) first)
        target (->> quads (filter #(= (owl "annotatedTarget") (::pi %))) first)]
    (merge
     (if (::oi source) {::si (::oi source)} {::sb (::ob source)})
     {::pi (::oi property)}
     (select-keys target [::zn ::oi ::ob ::ol ::di ::lt]))))

(defn annotation-targets
  "Given a set of the subjects for OWL annotations
   and a sequence of states,
   return a map from quads to vectors of subjects
   for the OWL annotations that annotation that quad."
  [annotations quads]
  (->> quads
       (filter #(contains? annotations (::sb %)))
       (group-by ::sb)
       (map (fn [[sb quads]] [(annotation-target quads) sb]))
       (remove nil?)
       (reduce
        (fn [coll [quad sb]]
          (update coll quad (fnil conj []) sb))
        {})))

; # Stanzas
;
; We often want to process all the quads for a given subject IRI.
; We call this the "stanza" for that subject IRI.
; When the stanza includes an RDF list or other anonynous constructs
; then we need to trace those branches back to their root subject IRI.

(defn objects-subjects
  "Given a sequence of quad maps,
   return a map from objects to subjects."
  [quads]
  (reduce
   (fn [coll {:keys [::si ::pi ::sb ::ob ::oi] :as quad}]
     (cond
       (and sb (= pi (owl "annotatedSource"))) (assoc coll sb (or ob oi))
       ob (assoc coll ob (or sb si))
       :else coll))
   {}
   quads))

(defn subjects-blank-objects
  "Given a sequence of quad maps,
   return a map from subjects to sequences of blank objects."
  [quads]
  (reduce
   (fn [coll {:keys [::si ::pi ::sb ::ob ::oi] :as quad}]
     (if ob
       (update coll (or sb si) (fnil conj []) ob)
       coll))
   {}
   quads))

(defn find-stanza
  "Given a map from objects to subjects and an subject,
   return it's highest subject or itself."
  [coll sb]
  (loop [zn sb]
    (if (find coll zn)
      (recur (get coll zn))
      zn)))

(defn assign-stanza
  "Given a map from objects to subjects and a quad,
   return the quad with its stanza assigned."
  [coll {:keys [::si ::sb] :as quad}]
  (if (or si sb)
    (assoc quad ::zn (if sb (find-stanza coll sb) si))
    quad))

(defn assign-stanzas
  "Given a sequence of quad maps,
   assume that blank node constructs are consecutive,
   and return a lazy sequence of quad maps with stanza assigned."
  [quads]
  (->> quads
       (partition-by (fn [{:keys [::sb ::ob]}] (boolean (or sb ob))))
       (mapcat #(map (partial assign-stanza (objects-subjects %)) %))))
