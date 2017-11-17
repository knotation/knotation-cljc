(ns org.knotation.link
  (:require [clojure.string :as string]

            [org.knotation.util :as util]
            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]))

(defn label->iri
  [env input]
  (when (string? input)
    (get-in env [::en/label-iri input])))

(defn wrapped-iri->iri
  [env input]
  (when (string? input)
    (when-let [[_ iri] (re-matches #"<(\S+)>" input)]
      iri)))

(defn http-url->iri
  [env input]
  (when (string? input)
    (when (re-matches #"https?://\S+" input)
      input)))

(defn curie->iri
  [env input]
  (when (string? input)
    (when-let [[_ prefix suffix] (re-matches #"(\S+):(\S+)" input)]
               ; TODO: (re-matches #"([a-zA-Z0-9]+):([^\s:/][^\s:\\]*)" input)]
      (when-let [iri (get-in env [::en/prefix-iri prefix])]
        (str iri suffix)))))

(defn wrapped-iri-or-bnode->node
  [input]
  (or (when-let [iri (wrapped-iri->iri {} input)]
        {::rdf/iri iri})
      (when (re-matches #"_:\S+" input)
        {::rdf/bnode input})))

(defn subject->iri
  [env input]
  (or (wrapped-iri->iri env input)
      (label->iri env input)
      (curie->iri env input)
      (http-url->iri env input)))

(defn graph->node
  [env input]
  (when input
    (or (when-let [iri (subject->iri env input)]
          {::rdf/iri iri})
        (when (re-matches #"_:\S+" input)
          {::rdf/bnode input}))))

(defn subject->node
  [env input]
  (or (when-let [iri (subject->iri env input)]
        {::rdf/iri iri})
      (when (re-matches #"_:\S+" input)
        {::rdf/bnode input})))

(defn predicate->iri
  [env input]
  (or (wrapped-iri->iri env input)
      (label->iri env input)
      (curie->iri env input)
      (http-url->iri env input)))

(defn datatype->iri
  [env input]
  (or (wrapped-iri->iri env input)
      (label->iri env input)
      (curie->iri env input)
      (http-url->iri env input)))

(defn object->node
  [env input]
  (or (when-let [iri (subject->iri env input)]
        {::rdf/iri iri})
      (when (re-matches #"_:\S+" input)
        {::rdf/bnode input})))

(defn find-prefix
  [env iri]
  (->> env
       ::en/iri-prefix
       (sort-by (comp count first) >)
       (filter
        (fn [[prefix-iri prefix]]
          (util/starts-with? iri prefix-iri)))
       first))

(defn iri->curie
  [env iri]
  (when-let [[prefix name] (find-prefix env iri)]
    (string/replace iri prefix (str name ":"))))

(defn iri->http-url
  [env iri]
  (when (re-matches #"https?://\S+" iri) iri))

(defn iri->wrapped-iri
  [env iri]
  (str "<" iri ">"))

(defn iri->curie-or-wrapped-iri
  [env iri]
  (or (iri->curie env iri)
      (iri->wrapped-iri env iri)))

(defn iri->name
  [env iri]
  (or
   (get-in env [::en/iri-label iri])
   (iri->curie env iri)
   (iri->http-url env iri)
   (iri->wrapped-iri env iri)))

(defn node->name
  [env {:keys [::rdf/iri ::rdf/bnode] :as node}]
  (or
   (when iri (iri->name env iri))
   bnode))
