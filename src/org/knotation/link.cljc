(ns org.knotation.link
  (:require [clojure.string :as string]
            [org.knotation.util :as util]
            [org.knotation.environment :as en]
            [org.knotation.rdf :as rdf])) ; TODO: eliminate this

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

(defn find-prefix
  [env iri]
  (->> env
       ::en/iri-prefix
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
  [env iri]
  (wrap-iri iri))

(defn iri->label
  [env iri]
  (get-in env [::en/iri-label iri]))

(defn iri->name
  [env iri]
  (or
   (iri->label env iri)
   (iri->curie env iri)
   (iri->http-url env iri)
   (iri->wrapped-iri env iri)))

(defn ->iri
  [env input]
  (or (wrapped-iri->iri env input)
      (label->iri env input)
      (curie->iri env input)
      (http-url->iri env input)))

(defn object->node
  [env input]
  (or (when-let [iri (->iri env input)]
        {:oi iri})
      (when (re-matches #"_:\S+" input)
        {:ob input})))
