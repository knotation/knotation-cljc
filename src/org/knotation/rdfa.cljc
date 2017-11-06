(ns org.knotation.rdfa
  (:require [org.knotation.state :as st]
            [org.knotation.link :as ln]
            [org.knotation.object :as ob]))

(defn quad->lines
  [env {:keys [predicate object] :as quad}]
  [(str
    "    "
    "<li>"
    "<a href=\""
    (:iri predicate)
    "\">"
    (ln/node->name env predicate)
    "</a>: "
    (if (:iri object)
      (str
       "<a href=\""
       (:iri object)
       "\">"
       (ln/node->name env object)
       "</a>")
      (:lexical object))
    "</li>")])

(defn state->lines
  [{:keys [env quads] :as state}]
  (mapcat (partial quad->lines env) quads))

(defn append
  [after before]
  (concat before after))

(defn subject-states->lines
  [states]
  (let [{:keys [subject] :as state} (first states)
        label (ln/node->name (-> states last :env) subject)
        link (str "<a href=\"" (:iri subject) "\">" (:iri subject) "</a>")]
    (->> states
         (mapcat state->lines)
         ;(concat [(str ": " (ln/node->name env-before subject))]))))
         (concat
          ["<div>"
           (str "  <p>" label "</p>")
           (str "  <p>" link "</p>")
           "  <ul>"])
         (append
          ["  </ul>"
           "</div>"]))))

(defn states->lines
  [states]
  (->> states
       (filter :quads)
       (partition-by :subject)
       (map subject-states->lines)
       (mapcat identity)))

(def example-quad-ex-text-foo
  {:graph nil
   :subject {:iri "https://example.com/foo"}
   :predicate {:iri "https://example.com/text"}
   :object {:lexical "Foo"}})

(def example-quad-rdfs-label-foo
  {:graph nil
   :subject {:iri "https://example.com/foo"}
   :predicate {:iri "http://www.w3.org/2000/01/rdf-schema#label"}
   :object {:lexical "Foo"}})

(def example-quad-homepage
  {:graph nil
   :subject {:iri "https://example.com/foo"}
   :predicate {:iri "https://example.com/homepage"}
   :object {:iri "https://example.com"}})

(def example-quad-default-datatype
  {:graph nil
   :subject {:iri "https://example.com/foo"}
   :predicate {:iri "https://knotation.org/predicate/default-datatype"}
   :object {:iri "https://knotation.org/datatype/link"}})

(def example-states
  [(assoc
    st/default-state
    :subject {:iri "https://example.com/foo"}
    :quads
    [example-quad-ex-text-foo])])
