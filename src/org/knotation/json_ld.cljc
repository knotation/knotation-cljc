(ns org.knotation.json-ld
  (:require [clojure.data.json :as json]
            [org.knotation.rdf :as rdf]
            [org.knotation.link :as ln]))

(defn render-object
  [env {:keys [::rdf/oi ::rdf/ob ::rdf/ol ::rdf/di ::rdf/lt]}]
  (cond
    oi
    {"@id" (or (ln/iri->curie env oi) oi)
     "iri" oi
     "label" (ln/iri->label env oi)}
    ob
    {"@id" ob}
    (and ol lt)
    {"@value" ol "@language" lt}
    (and ol di)
    {"@value" ol "@type" (or (ln/iri->name env di) di)}
    ol
    {"@value" ol}))

(defn render-context
  [env states]
  {"@context"
   (merge
    (->> states
         (filter :prefix)
         (map (juxt :prefix :iri))
         (into {}))
    (->> states
         (mapcat #(select-keys % [::rdf/si ::rdf/pi ::rdf/oi ::rdf/di]))
         vals
         (map (fn [iri]
                (let [label (ln/iri->label env iri)]
                  (when label
                    [label (ln/iri->curie env iri)]))))
                     ;{"@id" (ln/iri->curie env iri)
                     ; "iri" iri]))))
         (remove nil?)
         (into {})))})

(defn render-stanza-edn
  [env si states]
  (->> states
       (filter ::rdf/pi)
       (reduce
        (fn [coll {:keys [::rdf/pi] :as state}]
          (let [plabel (ln/iri->name env pi)]
            (assoc coll plabel (render-object env state))))
        (let [curie (ln/iri->curie env si)]
          {"@id" curie
           "iri" si
           "curie" curie}))
       (merge (render-context env states))))

(defn render-stanza
  [env si states]
  (json/write-str
   (render-stanza-edn env si states)
   :escape-slash false))
