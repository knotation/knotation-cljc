(ns org.knotation.rdf)

; # Namespaces

(def rdf (partial apply str "http://www.w3.org/1999/02/22-rdf-syntax-ns#"))
(def rdfs (partial apply str "http://www.w3.org/2000/01/rdf-schema#"))
(def xsd (partial apply str "http://www.w3.org/2001/XMLSchema#"))
(def owl (partial apply str "http://www.w3.org/2002/07/owl#"))
(def kn (partial apply str "https://knotation.org/"))
(def ex (partial apply str "http://example.com/"))

; # Blank Nodes

(defn blank?
  [s]
  (= "_:" (when (string? s) (subs s 0 2))))

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
  "Given a sequence of quad maps,
   return a lazy sequence of quad maps with sequential blank nodes."
  [quads]
  (->> quads
       (reductions
        (fn [[coll _] {:keys [:sb :ob] :as quad}]
          (let [[coll sb] (replace-blank-node coll sb)
                [coll ob] (replace-blank-node coll ob)]
            [coll (merge quad (when sb {:sb sb}) (when ob {:ob ob}))]))
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
       (filter #(= head (or (:si %) (:sb %))))
       (filter #(= (rdf "first") (:pi %)))
       first
       nil?
       not))

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
   (fn [coll {:keys [si pi sb ob oi]}]
     (cond
       ob (assoc coll ob (or sb si))
       (and sb (= pi (owl "annotatedSource"))) (assoc coll sb oi)
       :else coll))
   {}
   quads))

(defn find-stanza
  "Given a map from objects to subjects and an subject,
   return it's highest subject or itself."
  [coll sb]
  (loop [zi sb]
    (if (find coll zi)
      (recur (get coll zi))
      zi)))

(defn assign-stanza
  "Given a map from objects to subjects and a quad,
   return the quad with its stanza assigned."
  [coll {:keys [si sb] :as quad}]
  (if (or si sb)
    (assoc quad :zi (if sb (find-stanza coll sb) si))
    quad))

(defn assign-stanzas
  "Given a sequence of quad maps,
   assume that blank node constructs are consecutive,
   and return a lazy sequence of quad maps with stanza assigned."
  [quads]
  (->> quads
       (partition-by (fn [{:keys [sb ob]}] (boolean (or sb ob))))
       (mapcat #(map (partial assign-stanza (objects-subjects %)) %))))
