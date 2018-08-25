(ns org.knotation.ttl
  (:require [clojure.string :as string]

            [org.knotation.rdf :as rdf]
            [org.knotation.link :as ln]
            [org.knotation.format :as fm]))

(defn deep-line-count
  [tree]
  (->> tree flatten
       (filter string?)
       (filter #(string/starts-with? % "\n"))
       (map count) (reduce +)))

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

(defn -inner-render-subject
  [env triples s]
  (->> triples
       (filter #(= s (or (:si %) (:sb %))))
       (filter :pi)
       (map (partial render-statement env triples))
       (interpose [" ;" "\n" "  "])
       flatten))

(defn render-subject
  "Given an environment, a sequence of triple maps, and a subject node,
   return a sequence of strings representing the subject."
  [env triples s]
  (concat
   (if (rdf/blank? s) ["[ "] [(render-iri env s) "\n" "  "])
   (-inner-render-subject env triples s)
   (if (rdf/blank? s) [" ]"] [])))

(defn render-declaration
  [{:keys [prefix iri base] :as triple}]
  (cond
    (and prefix iri) (assoc triple :output {:parse ["@prefix " prefix ": <" iri "> ." "\n"] :line-count 1})
    base (assoc triple :output {:parse ["@base <" base "> ." "\n"] :line-count 1})
    :else triple))

(defn annotation-subjects
  [triples]
  (->> triples
       (filter #(= (rdf/owl "Annotation") (:oi %)))
       (map :sb)
       set))

(defn remove-annotations
  [triples]
  (let [subjects (annotation-subjects triples)]
    (->> triples
         (remove #(contains? subjects (:sb %)))
         (remove #(= :annotation (:event %))))))

(defn select-annotations
  [triples]
  (let [subjects (annotation-subjects triples)]
    (->> triples
         (filter #(contains? subjects (:sb %))))))

(defn render-annotation
  [env triples zi]
  (->> triples
       (filter #(= (:pi %) (rdf/rdf "type")))
       (filter #(= (:oi %) (rdf/owl "Axiom")))
       (map :sb)
       (concat [(-inner-render-subject env triples zi)])
       (map #(concat % [" ." "\n"]))
       (map #(concat [zi "\n" "  "] %))))

(defn render-stanza-annotations
  [env triples]
  (mapcat
   (fn [s]
     (let [trips (->> triples (filter #(= s (:sb %))))]
       (render-annotation env trips s)))
   (annotation-subjects triples)))

(defn render-stanza
  "Given an environment and a sequence of triple maps for a single stanza,
   return a sequence of strings representing the stanza,
   including any OWL Axioms."
  [env triples]
  (let [{:keys [zi]} (first triples)]
    (if zi
      (let [un-annotated (remove-annotations triples)
            stanza (->> triples
                        (filter #(= (:pi %) (rdf/rdf "type")))
                        (filter #(= (:oi %) (rdf/owl "Axiom")))
                        (map :sb)
                        (remove #(= zi %))
                        (map (partial render-subject env un-annotated))
                        (concat [(render-subject env un-annotated zi)])
                        (map #(concat % [" ." "\n"]))
                        (#(concat % (render-stanza-annotations env triples)))
                        (interpose "\n"))]
        (cons
         (assoc (first triples) :output {:parse stanza :line-count (deep-line-count stanza)})
         (rest triples)))
      (let [res (map render-declaration triples)]
        (concat
         (butlast res)
         (list (update-in (last res) [:output :parse] #(concat % ["\n"]))))))))

(defn render-stanzas
  "Given an environment and a sequence of triple maps for zero or more stanza,
   return a (possibly nested) sequence of strings representing the stanzas."
  [env triples]
  (->> triples
       (partition-by :zi)
       (map (partial render-stanza env))))

(defn number-output-lines
  [states]
  (reductions
   (fn [prev cur]
     (let [ln (get-in prev [:output :line-number] 0)
           out (:output cur)
           ct (get-in prev [:output :line-count])
           cct (deep-line-count (:parse out))]
       (assoc
        cur :output
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
       flatten))
