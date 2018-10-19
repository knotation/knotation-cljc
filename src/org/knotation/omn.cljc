(ns org.knotation.omn
  (:require [clojure.string :as string]
            [instaparse.core :as insta] ;; #?(:clj :refer :cljs :refer-macros) [defparser]
            [org.knotation.util :as util :refer [throw-exception]]
            [org.knotation.rdf :as rdf :refer [owl rdf]]
            [org.knotation.environment :as en]))

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

(def manchester-parser (insta/parser manchester-grammar))

(defn parse-class-expression
  [content]
  (let [result (manchester-parser content)]
    (when (insta/failure? result)
      (println result)
      (util/throw-exception "Manchester parser failure"))
    result))

(defn ->obj
  [env subtree]
  (let [elem (first subtree)
        obj (or (and (get subtree :sb) {:ob (get subtree :sb)})
                (and (map? subtree) (select-keys subtree [:ob :ol :oi]))
                (and (get elem :sb) {:ob (get elem :sb)})
                (and (map? elem) (select-keys elem [:ob :ol :oi]))
                (and (string? (first elem)) {:ob (first elem)})
                (util/error :invalid-object-extraction subtree))]
    (if (contains? obj :ol)
      {:oi (en/name->iri env (:ol obj))}
      obj)))

(declare read-class-expression)

(defn read-restriction
  [env parse restriction]
  (let [[_ left _ _ _ right] parse
        b (rdf/random-blank-node)
        left (read-class-expression env left)
        right (read-class-expression env right)]
    (concat
     [{:sb b :pi (rdf "type") :oi (owl "Restriction")}
      (merge {:sb b :pi (owl "onProperty")} (->obj env left))
      (merge {:sb b :pi restriction} (->obj env right))]
     (when (map? (first left)) left)
     (when (map? (first right)) right))))

(defn read-combination
  [env parse combination]
  (let [[_ left _ _ _ right] parse
        b1 (rdf/random-blank-node)
        b2 (rdf/random-blank-node)
        b3 (rdf/random-blank-node)
        left (read-class-expression env left)
        right (read-class-expression env right)]
    (concat
     [{:sb b1 :pi (rdf "type") :oi (owl "Class")}
      {:sb b1 :pi combination :ob b2}
      (merge {:sb b2 :pi (rdf "first")} (->obj env left))
      {:sb b2 :pi (rdf "rest") :ob b3}
      (merge {:sb b3 :pi (rdf "first")} (->obj env right))
      {:sb b3 :pi (rdf "rest") :oi (rdf "nil")}]
     (when (map? (first left)) left)
     (when (map? (first right)) right))))

(defn read-negation
  [env parse]
  (let [b (rdf/random-blank-node)
        target (read-class-expression env (last parse))
        ms [{:sb b :pi (rdf "type") :oi (owl "Class")}
            (merge
             {:sb b :pi (owl "complementOf")}
             (->obj env target))]]
    (concat ms (when (map? (first target)) target))))

(defn read-class-expression
  [env parse]
  (case (first parse)
    (:CLASS_EXPRESSION :OBJECT_PROPERTY_EXPRESSION)
    (read-class-expression env (->> parse (remove string?) (remove keyword?) first))

    :LABEL {:ol (nth parse 2)}
    :SOME (read-restriction env parse (owl "someValuesFrom"))
    :ONLY (read-restriction env parse (owl "allValuesFrom"))
    :CONJUNCTION (read-combination env parse (rdf "intersectionOf"))
    :DISJUNCTION (read-combination env parse (rdf "unionOf"))
    :NEGATION (read-negation env parse)
    (util/error :unsupported-manchester-form parse)))

(defn read-class-string
  [env string]
  (->> string
       parse-class-expression
       (read-class-expression env)))

;; (defn get-by-predicate
;;   [predicate quads]
;;   (->> quads
;;        (filter #(-> % ::rdf/predicate ::rdf/iri (= predicate)))
;;        first))

;; (defn list->branch
;;   [items]
;;   (loop [result {::rdf/iri (rdf/rdf "nil")}
;;          items (reverse items)]
;;     (if-not (first items)
;;       result
;;       (recur
;;        (assoc
;;         (rdf/random-blank-node)
;;         ::rdf/pairs
;;         [{::rdf/predicate {::rdf/iri (rdf/rdf "first")}
;;           ::rdf/object (first items)}
;;          {::rdf/predicate {::rdf/iri (rdf/rdf "rest")}
;;           ::rdf/object result}])
;;        (rest items)))))

;; (defn branch->list
;;   [quads]
;;   (loop [results []
;;          quads quads]
;;     (let [item (get-by-predicate (rdf/rdf "first") quads)
;;           link (get-by-predicate (rdf/rdf "rest") quads)]
;;       (if-not (and item link)
;;         results
;;         (recur
;;          (conj results (::rdf/object item))
;;          (-> link ::rdf/object ::rdf/pairs))))))

;;;;;;;;;;
;;;;;;;;;;
;;;;;;;;;;
;;;;;;;;;;
;;;;;;;;;;
;;;;;;;;;;
;; (defn read-class-expression
;;   [env content]
;;   (->> content
;;        parse-class-expression
;;        (convert-class-expression env)))

;; (defn interpose-many
;;   [seps coll]
;;   (->> coll
;;        (map vector)
;;        (interpose seps)
;;        (mapcat identity)))

;; (defn render-class-expression
;;   [env {:keys [::rdf/pairs] :as node}]
;;   (or
;;    (when-let [label (get-in env [::en/iri-label (::rdf/iri node)])]
;;      (if (re-find #"\s" label)
;;        [:LABEL "'" label "'"]
;;        [:LABEL "" label ""]))

;;    (when-let [neg (::rdf/object (get-by-predicate (rdf/owl "complementOf") pairs))]
;;      [:NEGATION "not" " " (render-class-expression env neg)])

;;    (when-let [union (get-by-predicate (rdf/owl "unionOf") pairs)]
;;      (->> union
;;           ::rdf/object
;;           ::rdf/pairs
;;           branch->list
;;           (map (partial render-class-expression env))
;;           (interpose-many [" " "or" " "])
;;           (concat [:DISJUNCTION])))

;;    (when-let [intersection (get-by-predicate (rdf/owl "intersectionOf") pairs)]
;;      (->> intersection
;;           ::rdf/object
;;           ::rdf/pairs
;;           branch->list
;;           (map (partial render-class-expression env))
;;           (interpose-many [" " "and" " "])
;;           (concat [:CONJUNCTION])))

;;    (when-let [prop (get-by-predicate (rdf/owl "onProperty") pairs)]
;;      (let [some (get-by-predicate (rdf/owl "someValuesFrom") pairs)
;;            only (get-by-predicate (rdf/owl "allValuesFrom") pairs)
;;            values (::rdf/object (or some only))]
;;        [(cond some :SOME only :ONLY)
;;         [:OBJECT_PROPERTY_EXPRESSION
;;          (render-class-expression env (::rdf/object prop))]
;;         " "
;;         (cond some "some" only "only")
;;         " "
;;         (if (::rdf/pairs values)
;;           [:CLASS_EXPRESSION
;;            "("
;;            (render-class-expression env values)
;;            ")"]
;;           [:CLASS_EXPRESSION
;;            (render-class-expression env values)])]))))

;; (defn write-class-expression
;;   [branch]
;;   (->> branch
;;        flatten
;;        (filter string?)
;;        (apply str)))
