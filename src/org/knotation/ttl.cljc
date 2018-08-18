(ns org.knotation.ttl
  (:require [clojure.string :as string]

            [org.knotation.rdf :as rdf]
            [org.knotation.link :as ln]
            [org.knotation.format :as fm]))

(def txt "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
@prefix owl: <http://www.w3.org/2002/07/owl#>
@prefix obo: <http://purl.obolibrary.org/obo/>
@prefix knd: <https://knotation.org/datatype/>
@prefix knp: <https://knotation.org/predicate/>
@prefix ex: <https://example.com/>

: rdfs:label
rdfs:label: label

: knd:link
label: link

: knd:omn
label: OWL Manchester Syntax

: knp:default-datatype
label: default datatype
default datatype; link: link

: rdf:type
label: type
default datatype: link

: rdfs:subClassOf
label: subclass of
default datatype: OWL Manchester Syntax

: obo:RO_0002162
label: in taxon

: obo:NCBITaxon_56313
label: Tyto alba

: obo:UBERON_0000033
label: head

: ex:owl-head
label: owl head
type: owl:Class
subclass of: head and ('in taxon' some 'Tyto alba')")
(require '[org.knotation.api :as ap])
(require '[org.knotation.format :as fmt])
(let [res (->> txt (ap/read-from :kn))]
  (->> res (fmt/render-states :ttl (ap/env-of res))
       (map #(dissoc % :org.knotation.environment/env))))

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
    (if (and (string/blank? x) (> (count x) 1))
      (str x "  ")
      x)))

(declare render-subject)

(defn render-lexical
  [ol]
  (if (re-find #"\n" ol)
    (str "\"\"\"" ol "\"\"\"")
    (str "\"" ol "\"")))

(defn render-object
  "Given an environment, a sequence of triple maps, and an object node,
   return a (possibly nested) sequence of strings representing the object,
   including nested lists and anonymous subjects."
  [env triples {:keys [oi ob ol di ln]}]
  (cond
    oi (render-iri env oi)

    (and ob (rdf/rdf-list? triples ob))
    (concat
     ["(" "\n" "    "]
     (->> (rdf/collect-list triples ob)
          (map (partial render-object env triples))
          (interpose ["\n" "  "])
          flatten
          indent)
     ["\n" "  " ")"])

    (and ob (rdf/rdf-anonymous-subject? triples ob))
    (indent (render-subject env triples ob))

    ob ob

    (and di (not= di (rdf/xsd "string")))
    (str (render-lexical ol) "^^" (render-iri env di))

    ln (str (render-lexical ol) "@" ln)

    ol (render-lexical ol)))

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
        (filter :pi)
        (map (partial render-statement env triples))
        (interpose [" ;" "\n" "  "])
        flatten)
   (if (rdf/blank? s) [" ]"] [])))

(defn render-declaration
  [{:keys [prefix iri base] :as triple}]
  (dissoc
   (cond
     (and prefix iri) (assoc triple :output {:parse ["@prefix " prefix ": <" iri "> ." "\n"]})
     base (assoc triple :output {:parse ["@base <" base "> ." "\n"]})
     :else triple)
   :org.knotation.environment/env))

(defn render-stanza
  "Given an environment and a sequence of triple maps for a single stanza,
   return a sequence of strings representing the stanza,
   including any OWL Axioms."
  [env triples]
  (let [{:keys [zi]} (first triples)]
    (if zi
      (let [res (->> triples
                     (filter #(= (:pi %) (rdf/rdf "type")))
                     (filter #(= (:oi %) (rdf/owl "Axiom")))
                     (map :sb)
                     (remove #(= zi %))
                     (map (partial render-subject env triples))
                     (concat [(render-subject env triples zi)])
                     (map #(concat % [" ." "\n"])))]
        (map #(dissoc % :org.knotation.environment/env)
             (cons (assoc (first triples) :output {:parse res})
                   (rest triples))))
      (->> triples
           (map render-declaration)
           (remove nil?)))))

(defn render-stanzas
  "Given an environment and a sequence of triple maps for zero or more stanza,
   return a (possibly nested) sequence of strings representing the stanzas."
  [env triples]
  (->> triples
       (partition-by :zi)
       (map (partial render-stanza env))))

(defn propagate-newlines
  [states]
  (map
   (fn [state]
     (if (= :blank (:event state))
       (assoc state :output (dissoc (:input state) :line-number))
       state))
   states))

(defn deep-line-count
  [stanza-tree]
  (->> stanza-tree flatten
       (filter #(string/starts-with? % "\n"))
       (map count) (reduce +)))

(defn number-output-lines
  [states]
  (reductions
   (fn [prev cur]
     (let [ln (get-in prev [:output :line-number] 0)
           out (:output cur)
           ct (get-in prev [:output :line-count] 0)
           cct (deep-line-count (:parse out))]
       (assoc cur :output
              (assoc
               out
               :line-number (if (zero? cct) ln (+ ln ct))
               :line-count (if (zero? cct) ct cct)))))
   states))

(defmethod fm/render-states
  :ttl
  [fmt env states]
  (->> states
       (render-stanzas env)
       flatten
       propagate-newlines
       number-output-lines))
