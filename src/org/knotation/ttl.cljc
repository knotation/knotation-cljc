(ns org.knotation.ttl
  (:require [org.knotation.rdf :as rdf]
            [org.knotation.link :as ln]))

(defn render-iri
  "Given an environment and an IRI string,
   return a CURIE or a wrapped IRI string."
  [env iri]
  (or (ln/iri->curie env iri)
      (ln/wrap-iri iri)))

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
  "Given an environment, a sequence of triple maps, and an object node,
   return a (possibly nested) sequence of strings representing the object,
   including nested lists and anonymous subjects."
  [env triples {:keys [oi ob ol dt ln]}]
  (cond
    oi (render-iri env oi)

    ol (str "\"" ol "\"")

    (rdf/rdf-list? triples ob)
    (concat
     ["(" "\n" "    "]
     (->> (rdf/collect-list triples ob)
          (map (partial render-object env triples))
          (interpose ["\n" "  "])
          flatten
          indent)
     ["\n" "  " ")"])

    (rdf/rdf-anonymous-subject? triples ob)
    (indent (render-subject env triples ob))

    ob ob))

(defn render-statement
  "Given an environment, a sequence of triple maps, and a triple to render,
   return a (possibly nested) sequence of strings representing the statement,
   including nested lists and anonymous subjects."
  [env triples {:keys [pi ob] :as triple}]
  (concat
   [(render-iri env pi)]
   (if (and ob (not (rdf/rdf-list? triples ob)))
     ["\n" "  "]
     [" "])
   [(render-object env triples triple)]))

(defn render-subject
  "Given an environment, a sequence of triple maps, and a subject node,
   return a sequence of strings representing the subject."
  [env triples s]
  (concat
   (if (rdf/blank? s) ["[ "] [(render-iri env s) "\n" "  "])
   (->> triples
        (filter #(= s (or (:si %) (:sb %))))
        (map (partial render-statement env triples))
        (interpose [" ;" "\n" "  "])
        flatten)
   (if (rdf/blank? s) [" ]"] [])))

(defn render-declaration
  [{:keys [prefix iri base] :as triple}]
  (cond
    (and prefix iri) ["@prefix " prefix ": <" iri "> .\n"]
    base ["@base <" base "> .\n"]
    :else []))

(defn render-stanza
  "Given an environment and a sequence of triple maps for a single stanza,
   return a sequence of strings representing the stanza,
   including any OWL Axioms."
  [env triples]
  (let [{:keys [zi]} (first triples)]
    (if zi
      (->> triples
           (filter #(= (:pi %) (rdf/rdf "type")))
           (filter #(= (:oi %) (rdf/owl "Axiom")))
           (map :sb)
           (remove #(= zi %))
           (map (partial render-subject env triples))
           (concat [(render-subject env triples zi)])
           (map #(concat % [" ."]))
           (interpose "\n\n")
           flatten)
      (->> triples
           (map render-declaration)
           flatten
           (#(concat % "\n"))))))

(defn render-stanzas
  "Given an environment and a sequence of triple maps for zero or more stanza,
   return a (possibly nested) sequence of strings representing the stanzas."
  [env triples]
  (->> triples
       (partition-by :zi)
       (map (partial render-stanza env))))
