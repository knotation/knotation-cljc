(ns org.knotation.info
  (:require [clojure.string :as string]
            [clojure.walk :as walk]

            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]))

(def max-length 50)

(def default ["No information available"])

(defn position
  [{:keys [::st/input] :as state}]
  (apply
   str
   (concat
    ["Line " (str (::st/line-number input))]
    (when (::st/source input)
      [" of " (::st/source input)]))))

(defn title
  [{:keys [::st/event] :as state}]
  (case event
    ::st/error "Error"
    ::st/comment "Comment"
    ::st/space "Space"
    ::st/prefix "Prefix"
    ::st/graph-start "Graph Start"
    ::st/graph-end "Graph End"
    ::st/subject-start "Subject Start"
    ::st/subject-end "Subject End"
    ::st/statement "Statement"
    ::st/header "Header"))

(defn subject-status
  [{:keys [::en/env ::rdf/subject] :as state}]
  [:p
   "Subject"
   (vec
    (concat
     [:ul]
     (when-let [label (get-in env [::en/iri-label (::rdf/iri subject)])]
       [[:li "Label: " label]])
     (when (::rdf/iri subject)
       [[:li "IRI: " (::rdf/iri subject)]])
     (when (::rdf/bnode subject)
       [[:li "Blank Node " (::rdf/bnode subject)]])))])

(defn predicate-status
  [{:keys [::en/env ::rdf/predicate] :as state}]
  (let [iri (::rdf/iri predicate)
        default-datatype (get-in env [::en/predicate-datatype iri])
        default-language (get-in env [::en/predicate-language iri])]
    [:p
     "Predicate"
     (vec
      (concat
       [:ul]
       (when-let [label (get-in env [::en/iri-label iri])]
         [[:li "Label: " label]])
       [[:li "IRI: " iri]]
       (when-let [label (get-in env [::en/iri-label default-datatype])]
         [[:li "Default Datatype Label: " label]])
       (when default-datatype
         [[:li "Default Datatype IRI: " default-datatype]])
       (when default-language
         [[:li "Default Language: " default-language]])))]))

(defn object-status
  [{:keys [::en/env ::rdf/object] :as state}]
  (let [{:keys [::rdf/iri ::rdf/bnode ::rdf/lexical ::rdf/language ::rdf/datatype]} object]
    [:p
     "Object"
     (vec
      (concat
       [:ul]
       (when-let [label (get-in state [::en/env ::en/iri-label iri])]
         [[:li "Label: " label]])
       (when iri [[:li "IRI: " iri]])
       (when bnode [[:li "Blank Node: " bnode]])
       (when lexical
         (cond
           (> (count lexical) max-length)
           [[:li "Lexical value (truncated): " (subs lexical 0 max-length) " ..."]]
           (re-find #"\n" lexical)
           [[:li "Lexical value (truncated): " (-> lexical string/split-lines first) " ..."]]
           :else
           [[:li "Lexical value: " lexical]]))
       (when language [[:li "Language: " language]])
       (when-let [label (get-in env [::en/iri-label datatype])]
         [[:li "Datatype Label: " label]])
       (when datatype [[:li "Datatype IRI: " datatype]])))]))

(defn status-message
  [{:keys [::st/event ::st/error ::st/comment] :as state}]
  (case event
    ::st/error [[:p (::st/error-message error)]]
    ::st/comment [[:p comment]]
    ::st/space [[:p "Blank line."]]
    ::st/prefix nil
    ::st/graph-start nil
    ::st/graph-end nil
    ::st/subject-start [(subject-status state)]
    ::st/subject-end [(subject-status state)]
    ::st/statement [(subject-status state) (predicate-status state) (object-status state)]
    ::st/header nil
    ["No status information available."]))

(defn help-message
  [{:keys [::st/event] :as state}]
  (case event
    ::st/error nil

    ::st/comment
    [[:p
      "Comments are lines starting with "
      [:code "#"]
      "."]
     [:p
      "Comments do not change the RDF content of a document. "
      "Use them to make notes for human readers."]]

    ::st/space
    [[:p "Spaces are just empty lines."]
     [:p "Use them to make text more readable for humans."]]

    ::st/prefix
    [[:p
      "Prefix lines start with "
      [:code "@prefix"]
      " then a short prefix,"
      " followed by "
      [:code ": "]
      ", and an absolute IRI."]
     [:p
      "A prefixed name begins with a prefix, then "
      [:code ":"]
      ", and a suffix. "
      "The prefixed name can expanded to an absolute IRI "
      "by appending the suffix."]
     [:p "Prefixes must be defined before they are used."]
     [:p "Use prefixes and prefixed names to be more concise."]]

    ::st/graph-start default

    ::st/graph-end default

    ::st/subject-start
    [[:p
      "Subject lines begin with "
      [:code ": "]
      ", then an identifier such as an IRI or prefixed name."]
     [:p
      "All the following statements are about this subject, "
      "until a new subject is declared. "
      "A subject line and the following statements "
      "is called a 'stanza'."]]

    ::st/subject-end
    [[:p
      "A subject stanza ends "
      "with a new subject line, "
      "or the end of the document."]]

    ::st/statement
    [[:p
      "Statement lines start with a predicate identifier, "
      "then optionally a "
      [:code "; "]
      " and datatype identifier, "
      "then "
      [:code ": "]
      " and the object. "
      "Either "
      [:code "predicate: object"]
      " or "
      [:code "predicate; datatype: object"]
      ". "
      "The predicate and datatype identifiers can be "
      "IRIs, prefixed names, or labels. "
      "The object will depend on the datatype."]
     [:p
      "Statements form RDF triples, using the current subject."]

     ; Predicate
     (case (get-in state [::rdf/predicate ::rdf/iri])
       "http://www.w3.org/2000/01/rdf-schema#label"
       [:p
        "The "
        [:code "rdfs:label"]
        " predicate is used to declare labels "
        "for use as identifiers."]

       "https://knotation.org/predicate/default-datatype"
       [:p
        "The "
        [:code "knp:default-datatype"]
        " predicate is used to declare a datatype "
        "that will be used in all following statements "
        "with that predicate, "
        "unless the statement specifies another datatype."]

       "https://knotation.org/predicate/default-language"
       [:p
        "The "
        [:code "knp:default-language"]
        " predicate is used to declare a language "
        "that will be used in all following statements "
        "with that predicate, "
        "unless the statement specifies another language."]

       nil)

     ; Datatype
     (case (get-in state [::rdf/object ::rdf/datatype])
       "https://knotation.org/datatype/link"
       [:p
        "The "
        [:code "knd:link"]
        " datatype declares that the object is an IRI. "
        "The object can be an IRI string, prefixed name, or label."]

       "https://knotation.org/datatype/omn"
       [:p
        "The "
        [:code "knd:omn"]
        " datatype declares that the object is an OWL class expression "
        "written in Manchester syntax."]

       nil)]

    ::st/header default

    default))

(defn status
  [state]
  (concat
   [[:h3 (title state)]]
   [[:p (position state) "."]]
   (status-message state)))

(defn help
  [state]
  (concat
   (status state)
   (help-message state)))

;(println (markdown (help org.knotation.kn-test/x)))

(defn node->markdown
  [item]
  (if (vector? item)
    (let [[tag & more] item]
      (case tag
        :h3 (concat ["### "] more)
        :p (concat ["\n\n"] more)
        :li (concat ["\n- "] more)
        :code (concat ["`"] more ["`"])
        more))
    item))

(defn markdown
  [hiccup]
  (->> hiccup
       (into [:div])
       (walk/postwalk node->markdown)
       flatten
       (apply str)))

(defn node->html
  [item]
  (if (vector? item)
    (let [[tag & more] item]
      (concat
       [(str "<" (name tag) ">")]
       more
       [(str "</" (name tag) ">")]))
    item))

(defn html
  [hiccup]
  (->> hiccup
       (into [:div])
       (walk/postwalk node->html)
       flatten
       (apply str)))

(defn node->text
  [item]
  (if (vector? item)
    (let [[tag & more] item]
      (case tag
        :p (concat ["\n\n"] more)
        :li (concat ["\n- "] more)
        :code (concat ["`"] more ["`"])
        more))
    item))

(defn text
  [hiccup]
  (->> hiccup
       (into [:div])
       (walk/postwalk node->markdown)
       flatten
       (apply str)))
