(ns org.knotation.ttl
  (:require [clojure.string :as string]

            [org.knotation.rdf :as rdf]
            [org.knotation.link :as ln]
            [org.knotation.format :as fm]))

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
  (cond
    (and prefix iri) ["@prefix " prefix ": <" iri "> ."]
    base ["@base <" base "> ."]
    :else nil))

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
           (interpose "\n\n"))
      (->> triples
           (map render-declaration)
           (remove nil?)
           (interpose "\n")))))

(defn render-stanzas
  "Given an environment and a sequence of triple maps for zero or more stanza,
   return a (possibly nested) sequence of strings representing the stanzas."
  [env triples]
  (->> triples
       ;; (remove
       ;;  #(contains?
       ;;    #{:blank :comment :subject-start :subject-end :graph-start :graph-end}
       ;;    (:event %)))
       (partition-by :zi)
       (map (partial render-stanza env))))

(defn stanza-line-count
  [stanza-tree]
  (->> stanza-tree flatten (filter #(string/starts-with? % "\n")) (map count) (reduce +)))

(defn merge-stanzas
  [triples stanzas]
  (flatten
   (map
    (fn [ts s newline]
      (cons
       (merge (first ts) {:output {:parse [s newline] :line-number 0}})
       (rest ts)))
    (partition-by :zi triples)
    stanzas
    (concat (repeat (- (count stanzas) 1) "\n") [""]))))

(defn number-output-lines
  [states]
  (reductions
   (fn [prev cur]
     (let [ln (get-in prev [:output :line-number])
           out (:output cur)
           ct (stanza-line-count (get-in prev [:output :parse]))]
       (assoc cur :output (assoc out :line-count ct :line-number (+ ln ct)))))
   states))

(defmethod fm/render-states
  :ttl
  [fmt env states]
  (->> states
       (render-stanzas env)
       (merge-stanzas states)
       number-output-lines))
