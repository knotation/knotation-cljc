(ns org.knotation.json-ld
  (:require [clojure.data.json :as json]
            [org.knotation.rdf :as rdf]
            [org.knotation.link :as ln]))

(defn render-object
  [env {:keys [oi ob ol dt ln]}]
  (cond
    oi
    {"@id" (or (ln/iri->curie env oi) oi)
     "iri" oi
     "label" (ln/iri->label env oi)}
    ob
    {"@id" ob}
    (and ol ln)
    {"@value" ol "@language" ln}
    (and ol dt)
    {"@value" ol "@type" (or (ln/iri->name env dt) dt)}
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
         (mapcat #(select-keys % [:si :pi :oi :dt]))
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
       (filter :pi)
       (reduce
        (fn [coll {:keys [pi] :as state}]
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
