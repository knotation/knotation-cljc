(ns org.knotation.environment-spec
  (:require [clojure.spec.alpha :as s]

            [org.knotation.rdf :as rdf]
            [org.knotation.rdf-spec]
            [org.knotation.environment :as en]))

(s/def ::en/label string?)
(s/def ::en/prefix (s/and string? #(re-matches #"\S+" %) #(not (re-matches #":" %))))
(s/def ::en/curie (s/and string? #(re-matches #"(\S+):(\S+)" %)))
(s/def ::en/wrapped-iri (s/and string? #(re-matches #"<(\S+)>" %)))

(s/def ::en/prefix-iri (s/map-of ::en/prefix ::rdf/iri))
(s/def ::en/iri-prefix (s/map-of ::rdf/iri ::en/prefix))
(s/def ::en/prefix-seq (s/coll-of ::en/prefix))
(s/def ::en/sorted-prefix-seq ::en/prefix-seq)

(s/def ::en/label-iri (s/map-of ::en/label ::rdf/iri))
(s/def ::en/iri-label (s/map-of ::rdf/iri ::en/label))
(s/def ::en/label-seq (s/coll-of ::en/label))

(s/def ::en/predicate-datatype (s/map-of ::rdf/predicate ::rdf/iri))
(s/def ::en/predicate-language (s/map-of ::rdf/predicate ::rdf/language))

(s/def ::en/template-content (s/map-of ::rdf/predicate string?))

(s/def ::en/env (s/keys :opt [::en/prefix-iri ::en/iri-prefix ::en/prefix-seq
                              ::en/label-iri ::en/iri-label ::en/label-seq
                              ::en/predicate-datatype ::en/predicate-language
                              ::en/template-content]))

;; # Update environment

(s/fdef en/add-base
        :args (s/cat :env ::en/env :base ::rdf/iri)
        :ret ::en/env)

(s/fdef en/add-prefix
        :args (s/cat :env ::en/env :prefix ::en/prefix :iri ::rdf/iri)
        :ret ::en/env)

(s/fdef en/add-label
        :args (s/cat :env ::en/env :label ::en/label :iri ::rdf/iri)
        :ret ::en/env)

(s/fdef en/set-datatype
        :args (s/cat :env ::en/env :predicate ::rdf/predicate :datatype ::rdf/datatype)
        :ret ::en/env)

(s/fdef en/get-datatype
        :args (s/cat :env ::en/env :predicate ::rdf/predicate)
        :ret (s/nilable ::rdf/datatype))

(s/fdef en/set-language
        :args (s/cat :env ::en/env :predicate ::rdf/predicate :language ::rdf/language)
        :ret ::en/env)

(s/fdef en/get-language
        :args (s/cat :env ::en/env :predicate ::rdf/predicate)
        :ret (s/nilable ::rdf/language))

(s/fdef en/set-template-content
        :args (s/cat :env ::en/env :template ::rdf/subject :content string?)
        :ret ::en/env)

(s/fdef en/get-template-content
        :args (s/cat :env ::en/env :template ::rdf/subject)
        :ret (s/nilable string?))

;; # Conversions

(s/fdef en/wrapped-iri?
        :args (s/cat :input string?)
        :ret boolean?)

(s/fdef en/http-url?
        :args (s/cat :input string?)
        :ret boolean?)

(s/fdef en/wrapped-iri->iri
        :args (s/cat :wrapped-iri string?)
        :ret (s/nilable ::rdf/iri))

(s/fdef en/label->iri
        :args (s/cat :env ::en/env :label ::en/label)
        :ret (s/nilable ::rdf/iri))

(s/fdef en/curie->iri
        :args (s/cat :env ::en/env :curie ::en/curie)
        :ret (s/nilable ::rdf/iri))

(s/fdef en/name->iri
        :args (s/cat :env ::en/env :input string?)
        :ret ::rdf/iri)

(s/fdef en/find-prefix
        :args (s/cat :env ::en/env :prefix ::en/prefix)
        :ret (s/nilable (s/tuple ::rdf/iri ::en/prefix)))

(s/fdef en/iri->curie
        :args (s/cat :env ::en/env :iri ::rdf/iri)
        :ret (s/nilable ::en/curie))

(s/fdef en/iri->http-url
        :args (s/cat :env ::en/env :iri ::rdf/iri)
        :ret (s/nilable ::rdf/iri))

(s/fdef en/wrap-iri
        :args (s/cat :iri ::rdf/iri)
        :ret ::en/wrapped-iri)

(s/fdef en/iri->wrapped-iri
        :args (s/cat :iri ::rdf/iri)
        :ret ::en/wrapped-iri)

(s/fdef en/iri->label
        :args (s/cat :env ::en/env :iri ::rdf/iri)
        :ret (s/nilable ::en/label))

(s/fdef en/iri->name
        :args (s/cat :env ::en/env :iri ::rdf/iri)
        :ret string?)
