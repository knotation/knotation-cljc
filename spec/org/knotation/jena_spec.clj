(ns org.knotation.jena-spec
  (:require [clojure.spec.alpha :as s]

            [org.knotation.state :as st]
            [org.knotation.state-spec]
            [org.knotation.jena :as jena]))

(s/fdef jena/read-input
        :args (s/cat :format string? :input #(instance? java.io.InputStream %))
        :ret ::st/states)

(s/fdef jena/read-string
        :args (s/cat :format string? :input string?)
        :ret ::st/states)
