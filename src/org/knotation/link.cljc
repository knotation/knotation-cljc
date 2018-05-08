(ns org.knotation.link
  (:require [clojure.string :as string]
            [org.knotation.util :as util]
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

(defn wrap-iri
  [iri]
  (str "<" iri ">"))

(defn iri->wrapped-iri
  [env iri]
  (wrap-iri iri))

(defn iri->name
  [env iri]
  (or
   (get-in env [::en/iri-label iri])
   (iri->curie env iri)
   (iri->http-url env iri)
   (iri->wrapped-iri env iri)))
