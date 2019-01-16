(ns org.knotation.omn
  (:require [clojure.string :as string]
            [instaparse.core :as insta] ;; #?(:clj :refer :cljs :refer-macros) [defparser]
            [org.knotation.util :as util]
            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]))

(def manchester-grammar "
CLASS_EXPRESSION = '(' SPACE? CLASS_EXPRESSION SPACE? ')' SPACE?
                 | DISJUNCTION
                 | CONJUNCTION
                 | NEGATION
                 | RESTRICTION
                 | INDIVIDUAL_LIST
                 | LABEL

DISJUNCTION = CLASS_EXPRESSION SPACE 'or'  SPACE CLASS_EXPRESSION
CONJUNCTION = CLASS_EXPRESSION SPACE 'and' SPACE CLASS_EXPRESSION
NEGATION = 'not' SPACE CLASS_EXPRESSION

<RESTRICTION> = SOME | ONLY
SOME = OBJECT_PROPERTY_EXPRESSION SPACE 'some' SPACE CLASS_EXPRESSION
ONLY = OBJECT_PROPERTY_EXPRESSION SPACE 'only' SPACE CLASS_EXPRESSION

OBJECT_PROPERTY_EXPRESSION = 'inverse' SPACE LABEL | LABEL
INDIVIDUAL_LIST = '{' SPACE? LABEL (SPACE? ',' SPACE? LABEL)* SPACE? '}'

LABEL = \"'\" #\"[^']+\" \"'\" | #'' #'(\\w|:)+' #''
<SPACE> = #'\\s+'")

(def manchester-parser (insta/parser manchester-grammar))

(defn parse-class-expression
  [content]
  (let [result (manchester-parser content)]
    (when (insta/failure? result)
      (println result)
      (util/throw-exception "Manchester parser failure"))
    result))

(declare read-class-expression)

(defn read-restriction
  [env parse restriction]
  (let [[_ left _ _ _ right] parse
        b (rdf/random-blank-node)
        left (read-class-expression env left)
        right (read-class-expression env right)]
    (concat
     [#::rdf{:ob b}
      #::rdf{:sb b :pi (rdf/rdf "type") :oi (rdf/owl "Restriction")}
      (merge #::rdf{:sb b :pi (rdf/owl "onProperty")} (first left))
      (merge #::rdf{:sb b :pi restriction} (first right))]
     (rest left)
     (rest right))))

(defn read-combination
  [env parse combination]
  (let [[_ left _ _ _ right] parse
        b1 (rdf/random-blank-node)
        b2 (rdf/random-blank-node)
        b3 (rdf/random-blank-node)
        left (read-class-expression env left)
        right (read-class-expression env right)]
    (concat
     [#::rdf{:ob b1}
      #::rdf{:sb b1 :pi (rdf/rdf "type") :oi (rdf/owl "Class")}
      #::rdf{:sb b1 :pi combination :ob b2}
      (merge #::rdf{:sb b2 :pi (rdf/rdf "first")} (first left))
      #::rdf{:sb b2 :pi (rdf/rdf "rest") :ob b3}
      (merge #::rdf{:sb b3 :pi (rdf/rdf "first")} (first right))
      #::rdf{:sb b3 :pi (rdf/rdf "rest") :oi (rdf/rdf "nil")}]
     (rest left)
     (rest right))))

(defn read-negation
  [env parse]
  (let [b (rdf/random-blank-node)
        target (read-class-expression env (last parse))]
    (concat
     [#::rdf{:ob b}
      #::rdf{:sb b :pi (rdf/rdf "type") :oi (rdf/owl "Class")}
      (merge #::rdf{:sb b :pi (rdf/owl "complementOf")} (first target))]
     (rest target))))

(defn read-individual-list
  [env parse]
  (let [b1 (rdf/random-blank-node)
        rdf-list (->> parse
                      (filter vector?)
                      (filter #(= :LABEL (first %)))
                      (map #(nth % 2))
                      (map #(en/name->iri env %))
                      (map #(if (rdf/blank? %) {::rdf/ob %} {::rdf/oi %}))
                      rdf/make-list)]
    (concat
     [#::rdf{:ob b1}
      #::rdf{:sb b1 :pi (rdf/rdf "type") :oi (rdf/owl "Class")}
      #::rdf{:sb b1 :pi (rdf/owl "oneOf") :ob (-> rdf-list first ::rdf/sb)}]
     rdf-list)))

(defn read-class-expression
  [env parse]
  (case (first parse)
    (:CLASS_EXPRESSION :OBJECT_PROPERTY_EXPRESSION)
    (read-class-expression env (->> parse (remove string?) (remove keyword?) first))

    :LABEL [{::rdf/oi (en/name->iri env (nth parse 2))}]
    :SOME (read-restriction env parse (rdf/owl "someValuesFrom"))
    :ONLY (read-restriction env parse (rdf/owl "allValuesFrom"))
    :CONJUNCTION (read-combination env parse (rdf/owl "intersectionOf"))
    :DISJUNCTION (read-combination env parse (rdf/owl "unionOf"))
    :NEGATION (read-negation env parse)
    :INDIVIDUAL_LIST (read-individual-list env parse)
    (util/error :unsupported-manchester-form parse)))

(defn read-class-string
  [env string]
  (->> string
       parse-class-expression
       (read-class-expression env)))

(defn find-state
  [states pi]
  (->> states
       (filter #(= pi (-> % ::rdf/quad ::rdf/pi)))
       first))

(defn sort-statements
  [coll]
  (loop [{:keys [::rdf/subjects ::depth] :as coll} coll]
    (if-let [subject (first subjects)]
      (recur
       (if (first (get coll subject))
         (let [coll (->> (get coll subject)
                         (map #(assoc % ::omn true))
                         (assoc coll subject))
               states (get coll subject)
               state (first states)
               rdf-type        (find-state states (rdf/rdf "type"))
               complement-of   (find-state states (rdf/owl "complementOf"))
               intersection-of (find-state states (rdf/owl "intersectionOf"))
               union-of        (find-state states (rdf/owl "unionOf"))
               on-property     (find-state states (rdf/owl "onProperty"))
               some-values     (find-state states (rdf/owl "someValuesFrom"))
               all-values      (find-state states (rdf/owl "allValuesFrom"))
               one-of          (find-state states (rdf/owl "oneOf"))
               first-item      (find-state states (rdf/rdf "first"))
               rest-item       (find-state states (rdf/rdf "rest"))]
           (cond
             complement-of
             (let [ob (-> complement-of ::rdf/quad ::rdf/ob)]
               (-> coll
                   (update ::st/states conj (assoc rdf-type ::st/silent true))
                   (update ::st/states conj (assoc complement-of
                                                   ::st/before
                                                   (concat
                                                    [[:keyword "not"]
                                                     [:space " "]]
                                                    (when ob [[:symbol "("]]))))
                   (update subject (partial remove #{rdf-type complement-of}))
                   (update ::depth (fnil inc 0))
                   (assoc ::rdf/subjects (concat
                                          (when ob [ob])
                                          subjects))))

             one-of
             (let [ob (-> one-of ::rdf/quad ::rdf/ob)]
               (-> coll
                   (update ::st/states conj (assoc rdf-type ::st/silent true))
                   (update ::st/states conj (assoc one-of ::st/before [[:symbol "{"]]))
                   (update subject (partial remove #{rdf-type one-of}))
                   (update ::depth (fnil inc 0))
                   (assoc ::rdf/subjects (concat
                                          (when ob [ob])
                                          subjects))))

             (or union-of intersection-of)
             (-> coll
                 (update ::st/states conj (assoc rdf-type ::st/silent true))
                 (update ::st/states conj (assoc (or union-of intersection-of) ::st/silent true))
                 (update subject (partial remove #{rdf-type union-of intersection-of}))
                 (assoc ::rdf/subjects (concat
                                        (when-let [ob (-> union-of ::rdf/quad ::rdf/ob)] [ob])
                                        (when-let [ob (-> intersection-of ::rdf/quad ::rdf/ob)] [ob])
                                        subjects)))

             on-property
             (let [obp (-> on-property ::rdf/quad ::rdf/ob)
                   obv (-> (or some-values all-values) ::rdf/quad ::rdf/ob)
                   keyword (concat
                            (when obp [[:symbol ")"]])
                            [[:space " "]
                             [:keyword (if some-values "some" "only")]
                             [:space " "]]
                            (when obv [[:symbol "("]]))]
               (-> coll
                   (update ::st/states conj (assoc on-property ::st/before (when obp [[:symbol "("]])))
                   (update ::st/states conj (assoc rdf-type ::st/exact keyword))
                   (update subject (partial remove #{rdf-type on-property}))
                   (assoc ::rdf/subjects (concat
                                          (when obp [obp])
                                          (when obv [obv])
                                          subjects))))

             (or some-values all-values)
             (let [obv (-> (or some-values all-values) ::rdf/quad ::rdf/ob)]
               (-> coll
                   (update ::st/states conj (assoc (or some-values all-values) ::st/after (when obv [[:symbol ")"]])))
                   (update subject (partial remove #{some-values all-values}))))

             first-item
             (if-let [ob (-> first-item ::rdf/quad ::rdf/ob)]
               (-> coll
                   (update ::st/states conj (assoc first-item ::st/silent true))
                   (update subject (partial remove #{first-item}))
                   (assoc ::rdf/subjects (concat [ob] subjects)))
               (-> coll
                   (update ::st/states conj first-item)
                   (update subject (partial remove #{first-item}))))

             rest-item
             (let [collection
                   (->> coll
                        ::st/states
                        reverse
                        (map ::rdf/quad)
                        (map ::rdf/pi)
                        (filter #{(rdf/owl "unionOf") (rdf/owl "intersectionOf") (rdf/owl "oneOf")})
                        first)]
               (if-let [ob (-> rest-item ::rdf/quad ::rdf/ob)]
                 (-> coll
                     (update ::st/states conj (assoc rest-item
                                                     ::st/exact
                                                     (condp = collection
                                                       (rdf/owl "unionOf") [[:space " "] [:keyword "or"] [:space " "]]
                                                       (rdf/owl "intersectionOf") [[:space " "] [:keyword "and"] [:space " "]]
                                                       (rdf/owl "oneOf") [[:symbol ","] [:space " "]])))
                     (update subject (partial remove #{rest-item}))
                     (assoc ::rdf/subjects (concat [ob] subjects)))
                 (if (and depth (> depth 0))
                   (-> coll
                       (update ::st/states conj (assoc rest-item ::st/exact [(if (= collection (rdf/owl "oneOf")) [:symbol "}"] [:symbol ")"])]))
                       (update subject (partial remove #{rest-item}))
                       (update ::depth dec))
                   (-> coll
                       (update ::st/states conj (assoc rest-item ::st/silent true))
                       (update subject (partial remove #{rest-item}))))))

             ; this should never be reached?
             :else
             (-> coll
                 (update ::st/states conj state)
                 (update subject rest))))
         ; no more states for this subject
         (-> coll
             (dissoc subject)
             (update ::rdf/subjects rest))))
      ; no more subjects
      (let [states (::st/states coll)
            last-state (last states)]
        (assoc
         coll
         ::st/states
         (conj (vec (butlast states))
               (-> last-state
                   (dissoc ::st/silent)
                   (assoc ::last true))))))))

(defn render-statement
  [{:keys [::st/event ::en/env ::st/silent ::st/exact ::st/before ::st/after ::rdf/quad] :as state}]
  (cond
    (not= ::st/statement event) nil
    silent nil
    exact exact
    :else
    (concat
     before
     (when (and (::rdf/oi quad) (not= (rdf/rdf "nil") (::rdf/oi quad)))
       (let [name (en/iri->name env (::rdf/oi quad))]
         (if (re-find #"\s" name)
           [[:symbol "'"]
            [:lexical name]
            [:symbol "'"]]
           [[:lexical name]])))
     after)))

(defn render-states
  [states]
  (->> states
       (group-by ::rdf/subject)
       (#(assoc %
                ::rdf/subjects (->> states (map ::rdf/subject) distinct)
                ::st/states []))
       sort-statements
       ::st/states
       (mapcat render-statement)
       flatten
       (filter string?)
       string/join))
