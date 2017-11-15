(ns org.knotation.api-spec
  (:require [clojure.spec.alpha :as s]
            [org.knotation.state :as st]
            [org.knotation.state-spec]
            [org.knotation.api :as api]))

(s/def ::api/operation-type keyword?)

(s/def ::api/operation
  (s/keys :req [::api/operation-type]
          :opt [::st/source ::st/format ::st/lines]))

(s/def ::api/operations (s/coll-of ::api/operation))

(s/def ::api/operation-function
  (s/fspec :args (s/cat :states ::st/states)
           :ret ::st/states))

(s/fdef api/operation-function
        :args (s/cat :operation ::api/operation)
        :ret ::api/operation-function)

(s/fdef api/run-operations
        :args (s/cat :operations ::api/operations)
        :ret ::st/states)
