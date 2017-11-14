(ns org.knotation.object-spec
  (:require [clojure.spec.alpha :as s]
            [org.knotation.rdf :as rdf]
            [org.knotation.rdf-spec]
            [org.knotation.environment :as en]
            [org.knotation.environment-spec]
            [org.knotation.object :as ob]))

(s/fdef string->object
        :args (s/cat :env ::en/env
                     :lt-or-dt ::rdf/language-tag-or-datatype
                     :content string?)
        :ret ::rdf/object)

(s/fdef nquads-literal->object
        :args (s/cat :content string?)
        :ret ::rdf/object)

(s/fdef nquads-object->object
        :args (s/cat :content string?)
        :ret ::rdf/object)

(s/fdef object->nquads-object
        :args (s/cat :object ::rdf/object)
        :ret string?)
