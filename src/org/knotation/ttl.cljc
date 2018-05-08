(ns org.knotation.ttl
  (:require [org.knotation.rdf :as rdf]))

(defn render-iri
  "Given an IRI string, return a wrapped IRI string."
  [iri]
  (str "<" iri ">"))

(defn render-link
  "Given a node, return a string representation."
  [link]
  (render-iri link))

(defn indent
  "Given a sequence of strings,
   replace indentation strings with longer indentation strings."
  [xs]
  (for [x xs]
    (if (and (clojure.string/blank? x) (> (count x) 1))
      (str x "  ")
      x)))

(declare render-subject)

(defn render-object
  "Given a sequence of triple maps and an object node,
   return a (possibly nested) sequence of strings representing the object,
   including nested lists and anonymous subjects."
  [triples {:keys [oi ob ol dt ln]}]
  (cond
    oi (render-iri oi)

    ol (str "\"" ol "\"")

    (rdf/rdf-list? triples ob)
    (concat
     ["(" "\n" "    "]
     (->> (rdf/collect-list triples ob)
          (map (partial render-object triples))
          (interpose ["\n" "  "])
          flatten
          indent)
     ["\n" "  " ")"])

    (rdf/rdf-anonymous-subject? triples ob)
    (indent (render-subject triples ob))

    ob ob))

(defn render-statement
  "Given a sequence of triple maps and a triple to render,
   return a (possibly nested) sequence of strings representing the statement,
   including nested lists and anonymous subjects."
  [triples {:keys [pi ob] :as triple}]
  (concat
   [(render-iri pi)]
   (if (and ob (not (rdf/rdf-list? triples ob)))
     ["\n" "  "]
     [" "])
   [(render-object triples triple)]))

(defn render-subject
  "Given a sequence of triple maps and a subject node,
   return a sequence of strings representing the subject."
  [triples s]
  (concat
   (if (rdf/blank? s) ["[ "] [(render-link s) "\n"])
   (->> triples
        (filter #(= s (or (:si %) (:sb %))))
        (map (partial render-statement triples))
        (interpose [" ;" "\n" "  "])
        flatten)
   (if (rdf/blank? s) [" ]"] [])))

(defn render-stanza
  "Given a sequence of triple maps for a single stanza,
   return a sequence of strings representing the stanza,
   including any OWL Axioms."
  [triples]
  (let [{:keys [zi]} (first triples)]
    (->> triples
         (filter #(= (:pi %) (rdf/rdf "type")))
         (filter #(= (:oi %) (rdf/owl "Axiom")))
         (map :sb)
         (remove #(= zi %))
         (map (partial render-subject triples))
         (concat [(render-subject triples zi)])
         (map #(concat % [" ."]))
         (interpose "\n\n")
         flatten)))

(defn render-stanzas
  "Given a sequence of triple maps for zero or more stanza,
   return a (possibly nested) sequence of strings representing the stanzas."
  [triples]
  (->> triples
       (partition-by :zi)
       (map render-stanza)))
