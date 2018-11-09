(ns org.knotation.clj-api-spec
  (:require [clojure.spec.alpha :as s]

            [org.knotation.state :as st]
            [org.knotation.kn-spec]
            [org.knotation.ttl-spec]
            [org.knotation.nq-spec]
            [org.knotation.jena-spec]
            [org.knotation.clj-api :as api]))

(s/fdef api/read-input
        :args (s/cat :input-format ::st/format :initial-state (s/nilable ::st/state) :input-stream #(instance? java.io.InputStream %))
        :ret ::st/states)
(s/fdef api/read-string
        :args (s/cat :input-format ::st/format :initial-state (s/nilable ::st/state) :content string?)
        :ret ::st/states)
(s/fdef api/read-path
        :args (s/cat :force-format (s/nilable ::st/format) :initial-state (s/nilable ::st/state) :path string?)
        :ret ::st/states)
(s/fdef api/read-paths
        :args (s/cat :force-format (s/nilable ::st/format) :initial-state (s/nilable ::st/state) :paths (s/coll-of string?))
        :ret ::st/states)
