(ns org.knotation.cljs-api-spec
	(:require [clojure.spec.alpha :as s]
						[org.knotation.state :as st]
						[org.knotation.kn-spec]
						[org.knotation.ttl-spec]
						[org.knotation.cljs-api :as api]))

(s/fdef api/read-input
				:args (s/cat :input-format ::st/format :initial-state (s/nilable ::st/state) :thing string? (every? string?))
				:ret :st/states)
(s/fdef api/read-string
				:args (s/cat :input-format ::st/format :initial-state (s/nilable ::st/state) :content string?))
