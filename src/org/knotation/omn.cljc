(ns org.knotation.omn
  (:require [clojure.string :as string]
            [instaparse.core :as insta] ;; #?(:clj :refer :cljs :refer-macros) [defparser]
            [org.knotation.util :as util :refer [throw-exception]]
            [org.knotation.rdf :as rdf]
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
        obj (or (and (get subtree ::rdf/sb) {:ob (get subtree ::rdf/sb)})
                (and (map? subtree) (select-keys subtree [::rdf/ob ::rdf/ol ::rdf/oi]))
                (and (get elem ::rdf/sb) {::rdf/ob (get elem ::rdf/sb)})
                (and (map? elem) (select-keys elem [::rdf/ob ::rdf/ol ::rdf/oi]))
                (and (string? (first elem)) {::rdf/ob (first elem)})
                (util/error :invalid-object-extraction subtree))]
    (if (contains? obj ::rdf/ol)
      {:oi (en/name->iri env (::rdf/ol obj))}
      obj)))

(declare read-class-expression)

(defn read-restriction
  [env parse restriction]
  (let [[_ left _ _ _ right] parse
        b (rdf/random-blank-node)
        left (read-class-expression env left)
        right (read-class-expression env right)]
    (concat
     [#::rdf{:sb b :pi (rdf/rdf "type") :oi (rdf/owl "Restriction")}
      (merge #::rdf{:sb b :pi (rdf/owl "onProperty")} (->obj env left))
      (merge #::rdf{:sb b :pi restriction} (->obj env right))]
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
     [#::rdf{:sb b1 :pi (rdf/rdf "type") :oi (rdf/owl "Class")}
      #::rdf{:sb b1 :pi combination :ob b2}
      (merge #::rdf{:sb b2 :pi (rdf/rdf "first")} (->obj env left))
      #::rdf{:sb b2 :pi (rdf/rdf "rest") :ob b3}
      (merge #::rdf{:sb b3 :pi (rdf/rdf "first")} (->obj env right))
      #::rdf{:sb b3 :pi (rdf/rdf "rest") :oi (rdf/rdf "nil")}]
     (when (map? (first left)) left)
     (when (map? (first right)) right))))

(defn read-negation
  [env parse]
  (let [b (rdf/random-blank-node)
        target (read-class-expression env (last parse))
        ms [#::rdf{:sb b :pi (rdf/rdf "type") :oi (rdf/owl "Class")}
            (merge
             #::rdf{:sb b :pi (rdf/owl "complementOf")}
             (->obj env target))]]
    (concat ms (when (map? (first target)) target))))

(defn read-class-expression
  [env parse]
  (case (first parse)
    (:CLASS_EXPRESSION :OBJECT_PROPERTY_EXPRESSION)
    (read-class-expression env (->> parse (remove string?) (remove keyword?) first))

    :LABEL {::rdf/oi (en/name->iri env (nth parse 2))}
    :SOME (read-restriction env parse (rdf/owl "someValuesFrom"))
    :ONLY (read-restriction env parse (rdf/owl "allValuesFrom"))
    :CONJUNCTION (read-combination env parse (rdf/rdf "intersectionOf"))
    :DISJUNCTION (read-combination env parse (rdf/rdf "unionOf"))
    :NEGATION (read-negation env parse)
    (util/error :unsupported-manchester-form parse)))

(defn read-class-string
  [env string]
  (->> string
       parse-class-expression
       (read-class-expression env)))
