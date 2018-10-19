(ns org.knotation.nq-spec
  (:require [clojure.spec.alpha :as s]

            [org.knotation.rdf :as rdf]
            [org.knotation.state :as st]
            [org.knotation.state-spec]
            [org.knotation.nq :as nq]))

(s/fdef nq/render-quad
        :args (s/cat :quad ::rdf/quad)
        :ret string?)

(s/fdef nq/render-state
        :args (s/cat :state ::st/state)
        :ret ::st/state)

(s/fdef nq/render-states
        :args (s/cat :states ::st/states)
        :ret ::st/states)
