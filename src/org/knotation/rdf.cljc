(ns org.knotation.rdf)

(def rdf (partial apply str "http://www.w3.org/1999/02/22-rdf-syntax-ns#"))
(def rdfs (partial apply str "http://www.w3.org/2000/01/rdf-schema#"))
(def xsd (partial apply str "http://www.w3.org/2001/XMLSchema#"))
(def owl (partial apply str "http://www.w3.org/2002/07/owl#"))
(def kn (partial apply str "https://knotation.org/"))
(def ex (partial apply str "http://example.com/"))

(defn bnode
  [& args]
  {::bnode (apply str "_:" args)})

(defn iri
  [& args]
  {::iri (apply str args)})

(defn literal
  [lexical]
  {::lexical lexical})

(defn blank?
  [{:keys [::bnode] :as node}]
  (boolean bnode))

(defn blank-object?
  [{:keys [::object] :as quad}]
  (blank? object))

(defn any-blank?
  [{:keys [::subject ::object] :as quad}]
  (or (blank? subject) (blank? object)))

(defn random-blank-node
  []
  (bnode
   #?(:clj (java.util.UUID/randomUUID)
      :cljs (random-uuid))))

(declare replace-blank-nodes)

(defn replace-blank-node
  [{:keys [::counter] :as coll} node]
  (if-let [b (::bnode node)]
    (if (find coll b)
      (assoc coll ::node (get coll b))
      (let [node (merge node (bnode counter))
            coll (-> coll
                     (update ::counter inc)
                     (assoc b node)
                     (assoc ::node node))]
        (if (::pairs node)
          (let [results (reductions replace-blank-nodes coll (::pairs node))
                coll (last results)
                pairs (map ::quad (rest results))
                node (assoc node ::pairs pairs)]
            (assoc coll ::node node))
          coll)))
    (assoc coll ::node node)))

(defn replace-blank-nodes
  [coll {:keys [::subject ::object] :as quad}]
  (let [coll (replace-blank-node coll subject)
        subject (::node coll)
        coll (replace-blank-node coll object)
        object (::node coll)]
    (assoc
     coll
     ::quad
     (merge
      quad
      (when subject {::subject subject})
      (when object {::object object})))))

(defn sequential-blank-nodes
  [quads]
  (->> quads
       (reductions
        replace-blank-nodes
        {::counter 1})
       rest
       (map ::quad)))

(defn strip-pairs
  [{:keys [::object] :as quad}]
  (assoc quad ::object (dissoc object ::pairs)))

(defn assign-subjects
  [{:keys [::pairs] :as object}]
  (->> pairs
       (map #(assoc % ::subject (dissoc object ::pairs)))
       (assoc object ::pairs)))

; TODO: This uses naive recursion.
; Using zippers or something would be a better idea.
(defn branch-object
  [object quads]
  (let [{pairs true quads false}
        (group-by #(= object (::subject %)) quads)
        [pairs quads]
        (reduce
         (fn [[pairs quads] {:keys [::predicate ::object] :as pair}]
           (if (blank? object)
             (let [[object quads] (branch-object object quads)
                   pair (assoc pair ::object object)]
               [(conj pairs pair) quads])
             [(conj pairs pair) quads]))
         [[] quads]
         pairs)]
    [(->> pairs
          (map #(dissoc % ::subject))
          (assoc object ::pairs))
     quads]))

(defn branch-quads
  [quads]
  (let [{anonymous true named false}
        (group-by #(blank? (::subject %)) quads)]
    (loop [results []
           named named
           anonymous anonymous]
      (let [{:keys [::object] :as quad} (first named)]
        (cond
          (nil? quad)
          (concat results anonymous)

          (blank? object)
          (let [[object anonymous] (branch-object object anonymous)
                quad (assoc quad ::object object)]
            (recur (conj results quad) (rest named) anonymous))

          :else
          (recur (conj results quad) (rest named) anonymous))))))

(defn unbranch-quad
  [quad]
  (loop [quads [(strip-pairs quad)]
         pairs (->> quad
                    ::object
                    assign-subjects
                    ::pairs)]
    (if-not (seq pairs)
      quads
      (recur
       (->> pairs
            (map strip-pairs)
            (into quads))
       (->> pairs
            (map ::object)
            (map assign-subjects)
            (mapcat ::pairs))))))
