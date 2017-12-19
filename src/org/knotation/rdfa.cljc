(ns org.knotation.rdfa
  (:require [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]
            [org.knotation.link :as ln]
            [org.knotation.object :as ob]
            [org.knotation.omn :as omn]
            [org.knotation.format :as fm]))

(defn render-quad
  [env {:keys [::rdf/predicate ::rdf/object] :as quad}]
  [(str
    "    "
    "<li>"
    "<a href=\""
    (::rdf/iri predicate)
    "\">"
    (ln/node->name env predicate)
    "</a>: "
    (cond
      (::rdf/iri object)
      (str
       "<a href=\""
       (::rdf/iri object)
       "\">"
       (ln/node->name env object)
       "</a>")
      (::rdf/bnode object)
      (->> object
           (omn/render-class-expression env)
           (omn/write-class-expression))
      :else
      (::rdf/lexical object))
    "</li>")])

(defn output-lines
  [state lines]
  (assoc
   state
   ::st/output
   {::st/format :rdfa
    ::st/lines lines}))

(defn render-state
  [{:keys [::st/mode ::st/event
           ::en/env ::rdf/quads
           ::rdf/subject]
    :as state}]
  (case (if (= :env mode) nil event)
    ::st/subject-start
    (let [link (str "<a href=\"" (::rdf/iri subject) "\">" (::rdf/iri subject) "</a>")]
      (output-lines
       state
       ["<div>"
        (str "  <p>" link "</p>")
        "  <ul>"]))

    ::st/subject-end
    (output-lines state ["  </ul>" "</div>"])

    ::st/statement
    (output-lines state (mapcat (partial render-quad env) quads))

    state))

(defn render-states
  [states]
  (->> states
       (map render-state)
       fm/number-output-lines))

(fm/register!
 {::fm/name :rdfa
  ::fm/description "RDFa format"
  ::fm/render render-states})
