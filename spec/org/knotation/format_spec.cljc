(ns org.knotation.format-spec
  (:require [clojure.spec.alpha :as s]
            [org.knotation.state :as st]
            [org.knotation.state-spec]
            [org.knotation.format :as fm]))

(s/def ::fm/name string?)
(s/def ::fm/description string?)

(s/def ::fm/read
  (s/fspec :args (s/cat :options ::fm/options
                        :state ::st/state
                        :lines ::st/lines)
           :ret ::st/states))

(s/def ::fm/convert
  (s/fspec :args (s/cat :options ::fm/options
                        :states ::st/states)
           :ret ::st/states))

(s/def ::fm/format
  (s/keys :req [::fm/name
                ::fm/description
                ::fm/read
                ::fm/convert]))

(d/fdef register!
        :args [:format ::fm/format]
        :rdf ::fm/format)
