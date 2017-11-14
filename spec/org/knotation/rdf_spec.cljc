(ns org.knotation.rdf-spec
  (:require [clojure.spec.alpha :as s]
            [org.knotation.rdf :as rdf]))

; TODO: generalize IRI
(s/def ::rdf/iri (s/and string?  #(re-matches #"https?://\S+" %)))
(s/def ::rdf/bnode (s/and string? #(re-matches #"_:\S+" %)))
(s/def ::rdf/lexical string?)
(s/def ::rdf/language-tag (s/and string? #(re-matches #"@\S+" %)))
(s/def ::rdf/language (s/and string? #(re-matches #"\S+" %)))
(s/def ::rdf/datatype ::rdf/iri)

(s/def ::rdf/language-tag-or-datatype
  (s/or :language ::rdf/language-tag
        :datatype ::rdf/datatype))

(s/def ::rdf/iri-node
  (s/and
   (s/keys :req [::rdf/iri])
   #(not-any? % [::rdf/bnode ::rdf/lexical ::rdf/language ::rdf/datatype])))

(s/def ::rdf/blank-node
  (s/and
   (s/keys :req [::rdf/bnode])
   #(not-any? % [::rdf/iri ::rdf/lexical ::rdf/language ::rdf/datatype])))

(s/def ::rdf/link-node
  (s/or
   :iri-node ::rdf/iri-node
   :blank-node ::rdf/blank-node))

(s/def ::rdf/plain-literal-node
  (s/and
   (s/keys :req [::rdf/lexical])
   #(not-any? % [::rdf/iri ::rdf/bnode ::rdf/language ::rdf/datatype])))

(s/def ::rdf/language-literal-node
  (s/and
   (s/keys :req [::rdf/lexical ::rdf/language])
   #(not-any? % [::rdf/iri ::rdf/bnode ::rdf/datatype])))

(s/def ::rdf/typed-literal-node
  (s/and
   (s/keys :req [::rdf/lexical ::rdf/datatype])
   #(not-any? % [::rdf/iri ::rdf/bnode ::rdf/language])))

(s/def ::rdf/literal-node
  (s/or
   :plain-literal ::rdf/plain-literal-node
   :language-literal ::rdf/language-literal-node
   :typed-literal ::rdf/typed-literal-node))

(s/def ::rdf/node
  (s/or
   :blank-node ::rdf/blank-node
   :iri-node ::rdf/iri-node
   :literal-node ::rdf/literal-node))

(s/def ::rdf/graph-iri (s/or :default-graph nil? :named-graph ::rdf/iri))
(s/def ::rdf/graph (s/or :default-graph nil? :named-graph ::rdf/iri-node))
(s/def ::rdf/subject ::rdf/link-node)
(s/def ::rdf/predicate ::rdf/iri-node)
(s/def ::rdf/object ::rdf/node)

(s/def ::rdf/triple (s/keys :req [::rdf/subject ::rdf/predicate ::rdf/object]))
(s/def ::rdf/triples (s/coll-of ::rdf/triple))

(s/def ::rdf/quad
  (s/keys :req [::rdf/graph ::rdf/subject ::rdf/predicate ::rdf/object]))
(s/def ::rdf/quads (s/coll-of ::rdf/quad))

(s/def ::rdf/graph-map
  (s/map-of ::rdf/subject
            (s/map-of ::rdf/predicate
                      (s/coll-of ::rdf/object))))
(s/def ::rdf/dataset-map
  (s/map-of ::rdf/graph
            (s/map-of ::rdf/subject
                      (s/map-of ::rdf/predicate
                                (s/coll-of ::rdf/object)))))

(s/def ::rdf/triple-seq
  (s/cat :subject ::rdf/subject
         :predicate ::rdf/predicate
         :object ::rdf/object))
(s/def ::rdf/triple-seqs (s/coll-of ::rdf/triple-seq))

(s/def ::rdf/quad-seq
  (s/cat :graph ::rdf/graph
         :subject ::rdf/subject
         :predicate ::rdf/predicate
         :object ::rdf/object))
(s/def ::rdf/quad-seqs (s/coll-of ::rdf/quad-seq))
