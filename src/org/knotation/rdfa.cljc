(ns org.knotation.rdfa
  (:require [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]
            [org.knotation.link :as ln]
            [org.knotation.object :as ob]
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
    (if (::rdf/iri object)
      (str
       "<a href=\""
       (::rdf/iri object)
       "\">"
       (ln/node->name env object)
       "</a>")
      (::rdf/lexical object))
    "</li>")])

(defn render-state
  [{:keys [::en/env ::rdf/quads ::st/output-line-count
           ::rdf/subject ::st/previous-subject]
    :or {output-line-count 0}
    :as state}]
  (let [lines
        (concat
         (when-not (= subject previous-subject)
           (let [label (ln/node->name env subject)
                 link (str "<a href=\"" (::rdf/iri subject) "\">" (::rdf/iri subject) "</a>")
                 lines ["<div>"
                        (str "  <p>" label "</p>")
                        (str "  <p>" link "</p>")
                        "  <ul>"]
                 closing ["  </ul>" "</div>"]]
             (cond
               (nil? previous-subject) lines
               (nil? subject) closing
               :else (concat closing lines))))
         (when quads
           (mapcat (partial render-quad env) quads)))]
    (if (> (count lines) 0)
      (assoc
       state
       ::st/output-line-count (+ output-line-count (count lines))
       ::st/output
       {::st/format :rdfa
        ::st/line-number (inc output-line-count)
        ::st/lines lines})
      state)))

(defn render-states
  [states]
  (->> (concat states [st/blank-state])
       (reductions
        (fn [previous-state input-state]
          (-> input-state
              (assoc ::st/output-line-count
                     (get previous-state ::st/output-line-count 0)
                     ::st/previous-subject
                     (::rdf/subject previous-state))
              render-state))
        st/blank-state)
       rest))
       ;butlast))

(fm/register!
 {::fm/name :rdfa
  ::fm/description "RDFa format"
  ::fm/render render-states})
