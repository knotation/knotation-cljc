(ns org.knotation.rdf)

(def rdf (partial apply str "http://www.w3.org/1999/02/22-rdf-syntax-ns#"))
(def rdfs (partial apply str "http://www.w3.org/2000/01/rdf-schema#"))
(def xsd (partial apply str "http://www.w3.org/2001/XMLSchema#"))
(def owl (partial apply str "http://www.w3.org/2002/07/owl#"))
(def kn (partial apply str "https://knotation.org/"))

(defn bnode
  [& args]
  {::bnode (apply str "_:" args)})

(defn iri
  [& args]
  {::iri (apply str args)})

(defn literal
  [lexical]
  {::lexical lexical})
