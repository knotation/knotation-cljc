(ns org.knotation.draw-io
  (:require [org.knotation.environment :as en]
            [org.knotation.rdf :as rdf]
            [org.knotation.state :as st]
            [clojure.string :as s]
            [clojure.data.xml :as xml]))

(def rdfs-label "http://www.w3.org/2000/01/rdf-schema#label")
(def rdf-type "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
(def owl-class "http://www.w3.org/2002/07/owl#Class")

(defn read-xml
  "Read a draw.io string to a vector of XML objects as maps."
  [string]
  (->> string
       xml/parse-str
       :content
       first
       :content))

(defn get-iris
  "Given an environment and a vector of XML objects, use any labels to get 
   existing IRIs. Return a map of the XML node ID to IRI."
  [env objects]
  (reduce
    (fn [m obj]
      (let [attrs (:attrs obj)]
        (if (and (contains? attrs :vertex) (contains? attrs :value))
          (let [id (:id attrs)
                label (:value attrs)]
            (if-let [iri (en/name->iri env label)]
              (assoc m id iri)
              m))
          m)))
    {} objects))

(defn is-class?
  "Given a map of XML attributes, determine if the node represents a class 
   based on the styles."
  [attrs]
  (let [styles (reduce
                (fn [m style]
                  (let [detail (s/split style #"=")]
                    (into m {(first detail) (second detail)})))
                {} (s/split (:style attrs) #";"))]
    (if (contains? styles "rounded")
      (not (= (get styles "rounded") "0"))
      false)))

(defn parse-object
  "Given an XML object, determine if it is a node or an edge. Return a triple 
   that represents that object or nil."
  [previous-state iris object]
  (let [env (st/update-env 
              (or (::en/env previous-state) en/default-env) 
                  previous-state)
        attrs (:attrs object)]
    (if (contains? attrs :edge)
      ;; handle an edge
      (let [subject (or (get iris (:source attrs)) (str "_:" (:source attrs)))
            target (or (get iris (:target attrs)) (str "_:" (:target attrs)))
            property (en/name->iri env (:value attrs))]
        [{::en/env env
          ::st/event ::st/statement
          ::rdf/quad #::rdf{:sb subject
                            :zn subject
                            :pi property
                            :oi target}}])
      ;; handle a node
      (if (contains? attrs :vertex)
        (let [id (:id attrs)
              subject (str "_:" id)
              label (:value attrs)]
          ;; only define the node if its not already defined
          ;; i.e., the label was used to get an IRI
          (if (not (contains? iris id))
            (if (is-class? attrs)
              ;; If it is a class, add rdf:type owl:Class
              (let [first-state {::en/env env
                                 ::st/event ::st/statement
                                 ::rdf/quad #::rdf{:sb subject
                                                   :zn subject
                                                   :pi rdf-type
                                                   :oi owl-class}}]
                [first-state
                 {::en/env (st/update-env env first-state)
                  ::st/event ::st/statement
                  ::rdf/quad #::rdf{:sb subject
                                    :zn subject
                                    :pi rdfs-label
                                    :ol label}}])
              [{::en/env env
                ::st/event ::st/statement
                ::rdf/quad #::rdf{:sb subject
                                  :zn subject
                                  :pi rdfs-label
                                  :ol label}}])
            nil))
        nil))))

(defn parse-objects
  "Given a vector of XML objects, parse that into a vector of triples."
  [initial-state objects]
  (let [env (or (::en/env initial-state) en/default-env)
        iris (get-iris env objects)]
    (remove nil? 
      (reduce 
        (fn [vect obj] 
          (->> obj
               (parse-object (or (last vect) initial-state) iris)
               st/assign-subject
               (concat vect)))
        [] objects))))

(defn read-lines
  "Given an initial state and a vector of lines, read the lines as draw.io XML.
   Return a vector of states."
  [initial-state lines]
  (->> lines
       (map s/trim)
       (s/join "")
       read-xml
       (parse-objects initial-state)
       st/assign-stanzas
       st/insert-subject-events
       st/insert-stanza-separators
       st/insert-stanza-events))
