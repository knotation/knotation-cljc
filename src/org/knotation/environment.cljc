(ns org.knotation.environment
  (:require [clojure.string :as string]
            [org.knotation.util :as util]
            [org.knotation.rdf :as rdf]))

(defn add-base
  [env base]
  (assoc-in env [::base] base))

(defn add-prefix
  [env prefix iri]
  (-> env
      (assoc-in [::prefix-iri prefix] iri)
      (assoc-in [::iri-prefix iri] prefix)
      (update ::prefix-seq (fnil conj []) prefix)))

(defn add-label
  [env label iri]
  (-> env
      (assoc-in [::label-iri label] iri)
      (assoc-in [::iri-label iri] label)
      (update ::label-seq (fnil conj []) label)))

(defn set-datatype
  [env predicate datatype]
  (-> env
      (assoc-in [::predicate-datatype predicate] datatype)))

(defn get-datatype
  [env predicate]
  (get-in env [::predicate-datatype predicate]))

(defn set-language
  [env predicate language]
  (-> env
      (assoc-in [::predicate-language predicate] language)))

(defn get-language
  [env predicate]
  (get-in env [::predicate-language predicate]))

(defn set-template-content
  [env template content]
  (-> env
      (assoc-in [::template-content template] content)))

(defn get-template-content
  [env template]
  (get-in env [::template-content template]))

(def blank-env {})

(def default-env
  (-> blank-env
      (add-prefix "rdf" (rdf/rdf))
      (add-prefix "rdfs" (rdf/rdfs))
      (add-prefix "xsd" (rdf/xsd))
      (add-prefix "owl" (rdf/owl))
      (add-prefix "kn" (rdf/kn))
      (add-label "label" (rdf/rdfs "label"))
      (add-label "type" (rdf/rdf "type"))
      (add-label "link" (rdf/kn "link"))
      (add-label "default datatype" (rdf/kn "default-datatype"))
      (set-datatype (rdf/rdf "type") (rdf/kn "link"))
      (set-datatype (rdf/kn "default-datatype") (rdf/kn "link"))))

(defn http-url?
  [input]
  (boolean
   (and
    (string? input)
    (re-matches #"https?://\S+" input))))

(defn wrapped-iri?
  [input]
  (boolean
   (and
    (string? input)
    (re-matches #"<\S+>" input))))

(defn wrapped-iri->iri
  [input]
  (when (wrapped-iri? input)
    (when-let [[_ iri] (re-matches #"<(\S+)>" input)]
      iri)))

(defn label->iri
  [env input]
  (when (string? input)
    (get-in env [::label-iri input])))

(defn curie->iri
  [env input]
  (when (string? input)
    (when-let [[_ prefix suffix] (re-matches #"(\S+):(\S+)" input)]
               ; TODO: (re-matches #"([a-zA-Z0-9]+):([^\s:/][^\s:\\]*)" input)]
      (when-let [iri (get-in env [::prefix-iri prefix])]
        (str iri suffix)))))

(defn name->iri
  [env input]
  (or (wrapped-iri->iri input)
      (label->iri env input)
      (curie->iri env input)
      (when (http-url? input) input)
      (util/throw-exception "Unknown name " input " in environment " env)))

(defn find-prefix
  "Given an environment and an IRI,
   return the pair of a matching prefix IRI and prefix name,
   or nil."
  [env iri]
  (->> env
       ::iri-prefix
       (sort-by (comp count first) >)
       (filter
        (fn [[prefix-iri prefix]]
          (string/starts-with? iri prefix-iri)))
       first))

(defn iri->curie
  [env iri]
  (when-let [[prefix name] (find-prefix env iri)]
    (string/replace iri prefix (str name ":"))))

(defn iri->http-url
  [env iri]
  (when (re-matches #"https?://\S+" iri) iri))

(defn wrap-iri
  [iri]
  (str "<" iri ">"))

(defn iri->wrapped-iri
  [iri]
  (wrap-iri iri))

(defn iri->label
  [env iri]
  (get-in env [::iri-label iri]))

(defn iri->name
  [env iri]
  (or
   (iri->label env iri)
   (iri->curie env iri)
   (when (http-url? iri) iri)
   (iri->wrapped-iri iri)))
