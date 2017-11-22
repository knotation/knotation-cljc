(ns org.knotation.omn
  (:require [clojure.string :as string]
            [instaparse.core :as insta]
            [org.knotation.util :as util]
            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.link :as ln]))

(def manchester-grammar "
CLASS_EXPRESSION = '(' SPACE? CLASS_EXPRESSION SPACE? ')' SPACE?
                 | DISJUNCTION
                 | CONJUNCTION
                 | NEGATION
                 | RESTRICTION
                 | LABEL

DISJUNCTION = CLASS_EXPRESSION SPACE 'or'  SPACE CLASS_EXPRESSION
CONJUNCTION = CLASS_EXPRESSION SPACE 'and' SPACE CLASS_EXPRESSION
NEGATION = 'not' SPACE (RESTRICTION | LABEL)

<RESTRICTION> = SOME | ONLY
SOME = OBJECT_PROPERTY_EXPRESSION SPACE 'some' SPACE CLASS_EXPRESSION
ONLY = OBJECT_PROPERTY_EXPRESSION SPACE 'only' SPACE CLASS_EXPRESSION

OBJECT_PROPERTY_EXPRESSION = 'inverse' SPACE LABEL | LABEL

LABEL = \"'\" #\"[^']+\" \"'\" | #'' #'\\w+' #''
<SPACE> = #'\\s+'")

(insta/defparser manchester-parser manchester-grammar)

(defn parse-class-expression
  [content]
  (let [result (manchester-parser content)]
    (when (insta/failure? result)
      (println result)
      (throw (Exception. "Manchester parser failure")))
    result))

(defn get-by-predicate
  [predicate quads]
  (->> quads
       (filter #(-> % ::rdf/predicate ::rdf/iri (= predicate)))
       first))

(defn list->branch
  [items]
  (loop [result {::rdf/iri (rdf/rdf "nil")}
         items (reverse items)]
    (if-not (first items)
      result
      (recur
       (assoc
        (rdf/random-blank-node)
        ::rdf/pairs
        [{::rdf/predicate {::rdf/iri (rdf/rdf "first")}
          ::rdf/object (first items)}
         {::rdf/predicate {::rdf/iri (rdf/rdf "rest")}
          ::rdf/object result}])
       (rest items)))))

(defn branch->list
  [quads]
  (loop [results []
         quads quads]
    (let [item (get-by-predicate (rdf/rdf "first") quads)
          link (get-by-predicate (rdf/rdf "rest") quads)]
      (if-not (and item link)
        results
        (recur
         (conj results (::rdf/object item))
         (-> link ::rdf/object ::rdf/pairs))))))

(defn convert-class-expression
  "Given an environmnet and a parse vector,
   return a node with optional ::rdf/pairs."
  [env parse]
  (case (first parse)
    (:CLASS_EXPRESSION :OBJECT_PROPERTY_EXPRESSION)
    (->> parse
         (remove string?)
         (remove keyword?)
         first
         (convert-class-expression env))

    :LABEL
    {::rdf/iri (ln/subject->iri env (nth parse 2))}

    :NEGATION
    (assoc
     (rdf/random-blank-node)
     ::rdf/pairs
     [{::rdf/predicate {::rdf/iri (rdf/rdf "type")}
       ::rdf/object {::rdf/iri (rdf/owl "Class")}}
      {::rdf/predicate {::rdf/iri (rdf/owl "complementOf")}
       ::rdf/object (convert-class-expression env (last parse))}])

    (:DISJUNCTION :CONJUNCTION)
    (assoc
     (rdf/random-blank-node)
     ::rdf/pairs
     [{::rdf/predicate {::rdf/iri (rdf/rdf "type")}
       ::rdf/object {::rdf/iri (rdf/owl "Class")}}
      {::rdf/predicate
       {::rdf/iri
        (rdf/owl
         (case (first parse)
           :DISJUNCTION "unionOf"
           :CONJUNCTION "intersectionOf"))}
       ::rdf/object
       (->> parse
            (filter vector?)
            (map (partial convert-class-expression env))
            list->branch)}])

    (:SOME :ONLY)
    (assoc
     (rdf/random-blank-node)
     ::rdf/pairs
     [{::rdf/predicate {::rdf/iri (rdf/rdf "type")}
       ::rdf/object {::rdf/iri (rdf/owl "Restriction")}}
      {::rdf/predicate {::rdf/iri (rdf/owl "onProperty")}
       ::rdf/object (convert-class-expression env (second parse))}
      {::rdf/predicate
       {::rdf/iri
        (rdf/owl
         (case (first parse)
           :SOME "someValuesFrom"
           :ONLY "allValuesFrom"))}
       ::rdf/object (convert-class-expression env (last parse))}])))

(defn read-class-expression
  [env content]
  (->> content
       parse-class-expression
       (convert-class-expression env)))

(defn interpose-many
  [seps coll]
  (->> coll
       (map vector)
       (interpose seps)
       (mapcat identity)))

(defn render-class-expression
  [env {:keys [::rdf/pairs] :as node}]
  (or
   (when-let [label (get-in env [::en/iri-label (::rdf/iri node)])]
     (if (re-find #"\s" label)
       [:LABEL "'" label "'"]
       [:LABEL "" label ""]))

   (when-let [neg (::rdf/object (get-by-predicate (rdf/owl "complementOf") pairs))]
     [:NEGATION "not" " " (render-class-expression env neg)])

   (when-let [union (get-by-predicate (rdf/owl "unionOf") pairs)]
     (->> union
          ::rdf/object
          ::rdf/pairs
          branch->list
          (map (partial render-class-expression env))
          (interpose-many [" " "or" " "])
          (concat [:DISJUNCTION])))

   (when-let [intersection (get-by-predicate (rdf/owl "intersectionOf") pairs)]
     (->> intersection
          ::rdf/object
          ::rdf/pairs
          branch->list
          (map (partial render-class-expression env))
          (interpose-many [" " "and" " "])
          (concat [:CONJUNCTION])))

   (when-let [prop (get-by-predicate (rdf/owl "onProperty") pairs)]
     (let [some (get-by-predicate (rdf/owl "someValuesFrom") pairs)
           only (get-by-predicate (rdf/owl "allValuesFrom") pairs)
           values (::rdf/object (or some only))]
       [(cond some :SOME only :ONLY)
        [:OBJECT_PROPERTY_EXPRESSION
         (render-class-expression env (::rdf/object prop))]
        " "
        (cond some "some" only "only")
        " "
        (if (::rdf/pairs values)
          [:CLASS_EXPRESSION
           "("
           (render-class-expression env values)
           ")"]
          [:CLASS_EXPRESSION
           (render-class-expression env values)])]))))

(defn write-class-expression
  [branch]
  (->> branch
       flatten
       (filter string?)
       (apply str)))
