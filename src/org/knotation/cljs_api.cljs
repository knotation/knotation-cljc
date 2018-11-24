(ns org.knotation.cljs-api
	(:refer-clojure :exclude [read-string])
	(:require [clojure.string :as string]
						[org.knotation.util :as util]
						[org.knotation.rdf :as rdf]
						[org.knotation.environment :as en]
						[org.knotation.state :as st]
						[org.knotation.kn :as kn]
						[org.knotation.tsv :as tsv]
						[org.knotation.ttl :as ttl]
						[org.knotation.api :as api]))

(defn path-format
	"Given a path string, return a format keyword or nil."
	[path]
	(when (string? path)
		(cond
			(string/ends-with? path ".kn")  :kn
      (string/ends-with? path ".tsv") :tsv
      (string/ends-with? path ".nt")  :nt
      (string/ends-with? path ".ttl") :ttl
      (string/ends-with? path ".rdf") :rdfxml
      (string/ends-with? path ".owl") :rdfxml
      (string/ends-with? path ".edn") :edn
			:else nil)))

(defn read-input
	"Given a format keyword, an initial state (or nil for the default state), and 
	 a thing to read from (string or collection of strings), return a lazy 
	 sequence or states."
	[input-format initial-state thing]
	(cond
		(string? thing)
		(read-string input-format initial-state thing)
		(coll? thing)
		(concat (map read-string thing))
		:else 
		(util/throw-exception 
			(format "Unable to read input type '%s'" (type thing)))))

(defn read-string
	"Given a format keyword, an initial state (or nil for the default state), and
	 a content string, return a lazy sequence of states."
	[input-format initial-state content]
	(let [initial-state (or initial-state st/default-state)]
		(case input-format
			; TODO: more read formats
			:kn (kn/read-lines initial-state (split "\n" content))
			(throw (util/throw-exception
				(format "Unsupported read format '%s'" input-format))))))

; Render Output

(defn render-output
	"Given a format keyword, an initial state (or nil for the default state), a 
	 sequence of state maps, return the output string."
	[fmt env states]
	(case fmt
		:edn (string/join "\n" (map pr-str states))
		:ttl (api/render-to :ttl env states)
		:kn (api/render-to :kn env states)
		(throw (util/throw-exception (format "Unsupported write format '%s" fmt)))))
