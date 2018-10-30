(ns org.knotation.ttl
  (:require [clojure.string :as string]

            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]
            [org.knotation.format :as fmt]))

(defn deep-line-count
  [tree]
  (->> tree flatten
       (filter string?)
       (filter #(string/starts-with? % "\n"))
       (map count) (reduce +)))

(defn add-to-output
  [triple parse]
  (assoc triple :output {:parse parse :line-count (deep-line-count parse)}))

(defn render-iri
  "Given an environment and an IRI string,
   return a CURIE or a wrapped IRI string."
  [env iri]
  (or (en/iri->curie env iri)
      (en/iri->wrapped-iri iri)))

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
  "Given an environment, a sequence of states, and an object node,
   return a (possibly nested) sequence of strings representing the object,
   including nested lists and anonymous subjects."
  [env states {::rdf/keys [si pi oi ob ol di lt] :as trip}]
  (cond
    oi (render-iri env oi)

    (and ob (rdf/rdf-list? states ob))
    (concat
     ["(" "\n" "    "]
     (->> (rdf/collect-list states ob)
          (map (partial render-object env states))
          (interpose ["\n" "  "])
          flatten
          indent)
     ["\n" "  " ")"])

    (and ob (rdf/rdf-anonymous-subject? states ob))
    (concat ["[ "] (render-subject env states ob) [" ]"])

    ob ob

    (and di (not (contains? #{(rdf/xsd "string") (rdf/kn "link")} di)))
    (str (render-lexical ol) "^^" (render-iri env di))

    lt (str (render-lexical ol) "@" lt)

    ol (render-lexical ol)))

(defn render-statement
  "Given an environment, a sequence of states, and a triple to render,
   return a (possibly nested) sequence of strings representing the statement,
   including nested lists and anonymous subjects."
  [env states {::rdf/keys [pi ob] :as triple}]
  (concat
   [(render-iri env pi)]
   (if (and ob (rdf/rdf-anonymous-subject? states ob))
     ["\n" "  "]
     [" "])
   [(render-object env states triple)]))

(defn render-subject
  "Given an environment, a sequence of states, and a subject node,
   return a sequence of strings representing the subject."
  [env states s]
  (->> states
       (filter #(= s (or (::rdf/si %) (::rdf/sb %))))
       (filter ::rdf/pi)
       (map (partial render-statement env states))
       (interpose [" ;" "\n" "  "])
       flatten))

(defn render-declaration
  [{:keys [prefix iri base] :as triple}]
  (cond
    (and prefix iri) (add-to-output triple ["@prefix " prefix ": <" iri "> ." "\n"])
    base (add-to-output triple ["@base <" base "> ." "\n"])
    :else triple))

(defn annotation-subjects
  [triples]
  (->> triples
       (filter #(= (rdf/owl "Annotation") (::rdf/oi %)))
       (map ::rdf/sb)))

(defn remove-annotations
  [triples]
  (let [subjects (set (annotation-subjects triples))]
    (->> triples
         (remove #(contains? subjects (::rdf/sb %)))
         (remove #(= ::st/annotation (::st/event %))))))

(defn render-annotation
  [env triples zn]
  (->> triples
       (filter #(= (::rdf/pi %) (rdf/rdf "type")))
       (filter #(= (::rdf/oi %) (rdf/owl "Axiom")))
       (map ::rdf/sb)
       (concat [(render-subject env triples zn)])
       (map #(concat % [" ." "\n"]))
       (map #(concat [zn "\n" "  "] %))))

(defn render-stanza-annotations
  [env triples]
  (mapcat
   (fn [s]
     (let [trips (->> triples (filter #(= s (::rdf/sb %))))]
       (render-annotation env trips s)))
   (annotation-subjects triples)))

(defn ensure-ending-newline
  [states]
  (if (= "\n" (->> states last :output :parse last))
    states
    (concat
     (butlast states)
     (list
      (update-in
       (last states)
       [:output :parse]
       #(concat % ["\n"]))))))

(defn render-stanza
  "Given an environment and a sequence of states for a single stanza,
   return a sequence of states with rendered :output."
  [env states]
  (let [{:keys [::rdf/zn]} (first states)]
    (if zn
      (let [un-annotated (remove-annotations states)
            stanza (->> states
                        (filter #(= (::rdf/pi %) (rdf/rdf "type")))
                        (filter #(= (::rdf/oi %) (rdf/owl "Axiom")))
                        (map ::rdf/sb)
                        (remove #(= zn %))
                        (map (partial render-subject env un-annotated))
                        (concat [(render-subject env un-annotated zn)])
                        (map #(concat
                               [(render-iri env zn) "\n" "  "]
                               % [" ." "\n"]))
                        (#(concat % (render-stanza-annotations env states)))

                        (interpose "\n"))]
        (ensure-ending-newline
         (cons (add-to-output (first states) stanza) (rest states))))
      (let [res (map render-declaration states)]
        (if (empty? (remove #(contains? #::st{:graph-start :graph-end :comment :blank} (::st/event %)) res))
          res
          (ensure-ending-newline res))))))

(defn render-stanzas
  "Given an environment and a sequence of states for zero or more stanzas,
   return a lazy sequence of states with rendered output."
  [env states]
  (->> states
       (partition-by ::rdf/zn)
       (mapcat (partial render-stanza env))))

(defn number-output-lines
  [states]
  (reductions
   (fn [prev cur]
     (let [ln (get-in prev [:output :line-number] 0)
           out (:output cur)
           ct (get-in prev [:output :line-count] 0)
           cct (deep-line-count (:parse out))]
       (assoc
        cur :output
        (assoc
         out
         :line-number (if (zero? cct) ln (+ ln ct))
         :line-count (if (zero? cct) ct cct)))))
   states))

(defn render-states
  [env states]
  (->> states
       (render-stanzas env)
       flatten
       ensure-ending-newline
       number-output-lines))

(defmethod fmt/render-states
  :ttl
  [fmt env states]
  (render-states env states))
