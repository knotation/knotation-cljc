(ns org.knotation.rdf-spec
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as string]

            [org.knotation.rdf :as rdf]))

(s/def ::rdf/iri (s/and string?  #(re-matches #"\S+" %) #(not (string/starts-with? % "_:"))))
(s/def ::rdf/bnode (s/and string? #(re-matches #"_:\S+" %)))
(s/def ::rdf/lexical string?)
(s/def ::rdf/language-tag (s/and string? #(re-matches #"@\S+" %)))

(s/def ::rdf/graph (s/nilable ::rdf/iri))
(s/def ::rdf/subject (s/or :iri ::rdf/iri :bnode ::rdf/bnode))
(s/def ::rdf/stanza ::rdf/subject)
(s/def ::rdf/predicate ::rdf/iri)
(s/def ::rdf/datatype ::rdf/iri)
(s/def ::rdf/language string?) ; TODO: could be tighter


(s/def ::rdf/gi ::rdf/graph)
(s/def ::rdf/zn ::rdf/stanza)
(s/def ::rdf/si ::rdf/iri)
(s/def ::rdf/sb ::rdf/bnode)
(s/def ::rdf/pi ::rdf/iri)
(s/def ::rdf/oi ::rdf/iri)
(s/def ::rdf/ob ::rdf/bnode)
(s/def ::rdf/ol ::rdf/lexical)
(s/def ::rdf/di ::rdf/iri)
(s/def ::rdf/lt ::rdf/language)

(s/def ::rdf/triple (s/keys :req [::rdf/pi]
                            :opt [::rdf/zn
                                  ::rdf/si ::rdf/sb
                                  ::rdf/oi ::rdf/ob ::rdf/ol
                                  ::rdf/di ::rdf/lt]))
(s/def ::rdf/triples (s/coll-of ::rdf/triple))

(s/def ::rdf/quad (s/keys :req [::rdf/pi]
                          :opt [::rdf/gi
                                ::rdf/zn
                                ::rdf/si ::rdf/sb
                                ::rdf/oi ::rdf/ob ::rdf/ol
                                ::rdf/di ::rdf/lt]))
(s/def ::rdf/quads (s/coll-of ::rdf/quad))

; # Blank Nodes

(s/fdef rdf/blank?
        :args (s/cat :string string?)
        :ret boolean?)

(s/fdef rdf/random-blank-node
        :ret ::rdf/bnode)

(s/fdef rdf/replace-blank-node
        :args (s/cat :coll map? :node (s/nilable ::rdf/subject))
        :ret (s/tuple map? (s/nilable string?)))

(s/fdef rdf/sequential-blank-nodes
        :args (s/cat :maps (s/coll-of map?))
        :ret (s/coll-of map?))

(s/fdef rdf/rdf-anonymous-subject?
        :args (s/cat :quads ::rdf/quads :subject ::rdf/bnode)
        :ret boolean?)

; # RDF Lists

(s/fdef rdf/rdf-list?
        :args (s/cat :quads ::rdf/quads :head ::rdf/subject)
        :ret boolean?)

(s/fdef rdf/collect-list
        :args (s/cat :quads ::rdf/quads :head ::rdf/subject)
        :ret ::rdf/quads)

(s/def ::rdf/objects-subjects (s/map-of ::rdf/bnode ::rdf/subject))

;; # OWL Annotations

(s/fdef rdf/annotation-subjects
        :args (s/cat :quads ::rdf/quads)
        :ret (s/coll-of ::rdf/bnode :type set?))

(s/fdef rdf/annotation-target
        :args (s/cat :quads ::rdf/quads)
        :ret ::rdf/quad)

(s/fdef rdf/annotation-targets
        :args (s/cat :annotations (s/coll-of ::rdf/bnode :type set?) :quads ::rdf/quads)
        :ret (s/map-of ::rdf/quad (s/coll-of ::rdf/bnode :type vector?)))

;; # Stanzas

(s/fdef rdf/objects-subjects
        :args (s/cat :quads ::rdf/quads)
        :ret ::rdf/objects-subjects)

(s/fdef rdf/find-stanza
        :args (s/cat :objects-subjects ::rdf/objects-subjects :subject ::rdf/bnode)
        :ret ::rdf/subject)

(s/fdef rdf/assign-stanza
        :args (s/cat :objects-subjects ::rdf/objects-subjects :quad ::rdf/quad)
        :ret ::rdf/quad)

(s/fdef rdf/assign-stanzas
        :args (s/cat :quads ::rdf/quads)
        :ret ::rdf/quads)
