(ns org.knotation.rdf)

(def rdf (partial apply str "http://www.w3.org/1999/02/22-rdf-syntax-ns#"))
(def rdfs (partial apply str "http://www.w3.org/2000/01/rdf-schema#"))
(def xsd (partial apply str "http://www.w3.org/2001/XMLSchema#"))
(def owl (partial apply str "http://www.w3.org/2002/07/owl#"))
(def kn (partial apply str "https://knotation.org/"))
(def ex (partial apply str "http://example.com/"))

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
