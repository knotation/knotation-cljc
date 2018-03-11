(ns org.knotation.rdfa
  (:require [ring.util.codec :as codec]

            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]
            [org.knotation.link :as ln]
            [org.knotation.object :as ob]
            [org.knotation.omn :as omn]
            [org.knotation.format :as fm]))

(defn render-link
  [env node]
  (str
   "<a href=\"?iri="
   (codec/url-encode (::rdf/iri node))
   "\">"
   (ln/node->name env node)
   "</a>"))

(defn render-subject
  [env {:keys [::rdf/subject] :as quad}]
  (render-link env subject))

(defn render-predicate
  [env {:keys [::rdf/predicate] :as quad}]
  (render-link env predicate))

(defn render-object
  [env {:keys [::rdf/predicate ::rdf/object] :as quad}]
  (cond
    (::rdf/iri object)
    (str
     "<a rel=\""
     (::rdf/iri predicate)
     "\" resource=\""
     (::rdf/iri object)
     "\" href=\"?iri="
     (codec/url-encode (::rdf/iri object))
     "\">"
     (ln/node->name env object)
     "</a>")
    ; TODO: Fix this
    (::rdf/bnode object)
    (str
     "<span property=\""
     (::rdf/iri predicate)
     "\">"
     (->> object
          (omn/render-class-expression env)
          (omn/write-class-expression))
     "</span>")
    :else
    (str
     "<span property=\""
     (::rdf/iri predicate)
     "\">"
     (::rdf/lexical object)
     "</span>")))

(defn render-pair
  [env quad]
  (str
   "<strong>"
   (render-predicate env quad)
   "</strong> "
   (render-object env quad)))

(defn render-triple
  [env quad]
  (str
   (render-subject env quad)
   " <strong>"
   (render-predicate env quad)
   "</strong> "
   (render-object env quad)))

(defn render-pair-row
  [env quad]
  (str
   "<tr>"
   "<td>"
   (render-predicate env quad)
   "</td>"
   "<td>"
   (render-object env quad)
   "</td>"
   "</tr>"))

(defn render-triple-row
  [env {:keys [::rdf/subject] :as quad}]
  (str
   "<tr resource=\""
   (::rdf/iri subject)
   "\">"
   "<td>"
   (render-subject env quad)
   "</td>"
   "<td>"
   (render-predicate env quad)
   "</td>"
   "<td>"
   (render-object env quad)
   "</td>"
   "</tr>"))

(defn render-list-item
  [env quad]
  [(str
    "    "
    "<li>"
    (render-pair env quad)
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
           ::en/env
           ::rdf/subject]
    :as state}]
  (case (if (= :env mode) nil event)
    ::st/subject-start
    (let [link (str "<a href=\"" (::rdf/iri subject) "\">" (::rdf/iri subject) "</a>")]
      (output-lines
       state
       ["<div>"
        (str "  <p>" link "</p>")
        (str "  <ul resource=\"" (::rdf/iri subject) "\">")]))

    ::st/subject-end
    (output-lines state ["  </ul>" "</div>"])

    ::st/statement
    (output-lines state (render-list-item env state))

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
