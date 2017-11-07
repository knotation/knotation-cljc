(ns org.knotation.link
  (:require [clojure.string :as string]

            [org.knotation.util :as util]))

(defn label->iri
  [env input]
  (when (string? input)
    (get-in env [:label-iri input])))

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
      (when-let [iri (get-in env [:prefix-iri prefix])]
        (str iri suffix)))))

(defn wrapped-iri-or-bnode->node
  [input]
  (or (when-let [iri (wrapped-iri->iri {} input)]
        {:iri iri})
      (when (re-matches #"_:\S+" input)
        {:bnode input})))

(defn subject->iri
  [env input]
  (or (wrapped-iri->iri env input)
      (label->iri env input)
      (curie->iri env input)
      (http-url->iri env input)))

(defn subject->node
  [env input]
  (or (when-let [iri (subject->iri env input)]
        {:iri iri})
      (when (re-matches #"_:\S+" input)
        {:bnode input})))

(defn predicate->iri
  [env input]
  (or (wrapped-iri->iri env input)
      (label->iri env input)
      (curie->iri env input)
      (http-url->iri env input)))

(defn object->node
  [env input]
  (or (when-let [iri (subject->iri env input)]
        {:iri iri})
      (when (re-matches #"_:\S+" input)
        {:bnode input})))

(defn find-prefix
  [env iri]
  (->> env
       :iri-prefix
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

(defn iri->name
  [env iri]
  (or
   (get-in env [:iri-label iri])
   (iri->curie env iri)
   (iri->http-url env iri)
   (iri->wrapped-iri env iri)))

(defn node->name
  [env {:keys [iri bnode] :as node}]
  (or
   (when iri (iri->name env iri))
   bnode))
