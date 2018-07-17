(ns org.knotation.kn
  (:require [clojure.string :as string]
            [org.knotation.util :as util]
            [instaparse.core :as insta]
            #?(:clj [org.knotation.util :as util :refer [handler-case]]
               :cljs [org.knotation.util :as util])
            #?(:cljs [org.knotation.util-macros-cljs :refer-macros [handler-case]])
            [org.knotation.rdf :as rdf :refer [owl rdf]]
            [org.knotation.environment :as en]
            [org.knotation.link :as ln]
            [org.knotation.format :as fm]))

; A blank line contains nothing except an optional line ending.

(defn parse-blank
  [line]
  (if (re-matches #"\n?" line)
    [::blank-line
     [:eol "\n"]]
    (util/error :not-a-blank-line line)))

(defn read-blank
  [env parse]
  {:event :blank})

(defn render-blank
  [env state]
  [::blank-line
   [:eol "\n"]])

; A comment is a line starting with '#'.

(defn parse-comment
  [line]
  (if-let [[_ comment] (re-matches #"#(.*)\n?" line)]
    [::comment-line
     [:symbol "#"]
     [:comment comment]
     [:eol "\n"]]
    (util/error :not-a-comment-line line)))

(defn read-comment
  [env parse]
  (if-let [comment (-> parse fm/parse-map :comment)]
    {:event :comment
     :comment comment}
    (util/error :not-a-comment-parse parse)))

(defn render-comment
  [env {:keys [comment] :as state}]
  (if comment
    [::comment-line
     [:symbol "#"]
     [:comment comment]
     [:eol "\n"]]
    (util/error :not-a-comment-state state)))

; A prefix declaration is a line starting with '@prefix '

(defn parse-prefix
  [line]
  (if-let [[_ prefix iri] (re-matches #"@prefix (\S+):\s+<(\S+)>\s*\n?" line)]
    [::prefix-line
     [:symbol "@"]
     [:keyword "prefix"]
     [:space " "]
     [:prefix prefix]
     [:symbol ":"]
     [:space " "]
     [:symbol "<"]
     [:iri iri]
     [:symbol ">"]
     [:space " "]
     [:eol "\n"]]
    (util/error :not-a-prefix-line line)))

(defn read-prefix
  [env parse]
  (let [{:keys [keyword prefix iri]} (fm/parse-map parse)]
    (if (and (= keyword "prefix") prefix iri)
      {:event :prefix
       :prefix prefix
       :iri iri}
      (util/error :not-a-prefix-parse parse))))

(defn render-prefix
  [env {:keys [prefix iri] :as state}]
  (if (and prefix iri)
    [::prefix-line
     [:symbol "@"]
     [:keyword "prefix"]
     [:space " "]
     [:prefix prefix]
     [:symbol ":"]
     [:space " "]
     [:symbol "<"]
     [:iri iri]
     [:symbol ">"]
     [:space ""]
     [:eol "\n"]]
    (util/error :not-a-prefix-state state)))

; A declaration is one of: prefix

(defn parse-declaration
  [line]
  (cond
    (string/starts-with? line "@prefix ")
    (parse-prefix line)
    :else
    (util/error :unrecognized-declaration-line line)))

; A subject line start with ': '

(defn parse-subject
  [line]
  (let [[_ name] (re-matches #":\s+(.*)\n?" line)]
    (if name
      [::subject-line
       [:symbol ":"]
       [:space " "]
       [:name name]
       [:eol "\n"]]
      (util/error :not-a-subject-line line))))

(defn read-subject
  [env parse]
  (if-let [name (-> parse fm/parse-map :name)]
    (if-let [iri (ln/->iri env name)]
      {:event :subject-start
       :si iri}
      (util/error :unrecognized-name name))
    (util/error :not-a-subject-parse parse)))

(defn render-subject
  [env {:keys [si sb] :as state}]
  (cond
    sb
    [::subject-line
     [:symbol ":"]
     [:space " "]
     [:name sb]
     [:eol "\n"]]
    si
    (if-let [name (ln/iri->name env si)]
      [::subject-line
       [:symbol ":"]
       [:space " "]
       [:name name]
       [:eol "\n"]]
      (util/error :invalid-iri name))
    :else
    (util/error :not-a-subject-state state)))

; A statement

(defn parse-indented
  [line]
  (if-let [[_ lexical] (re-matches #" (.*)\n?" line)]
    [::indented-line
     [:space " "]
     [:lexical lexical]
     [:eol "\n"]]
    (util/error :not-an-indented-line line)))

(defn parse-statement
  [line]
  (if-let [[_ pd lexical] (re-matches #"(.*?): (.*)\n?" line)]
    (if-let [[_ predicate _ datatype] (re-matches #"(.*?)(; (.*))?" pd)]
      (if datatype
        [::statement-line
         [:name predicate]
         [:symbol ";"]
         [:space " "]
         [:name datatype]
         [:symbol ":"]
         [:space " "]
         [:lexical lexical]
         [:eol "\n"]]
        [::statement-line
         [:name predicate]
         [:symbol ":"]
         [:space " "]
         [:lexical lexical]
         [:eol "\n"]])
      (util/error :invalid-predicate-datatype pd))
    (util/error :not-a-statement line)))

; An annotation is a line starting with some number of >
(defn parse-annotation
  [line]
  (if-let [[_ arrows annotation] (re-matches #"(>+) (.*)\n?" line)]
    (vec
     (concat
      [::annotation-line
       [:symbol arrows]
       [:space " "]]
      (rest (parse-statement
             (subs line (+ (count arrows) 1))))))
    (util/error :not-a-statement line)))

(defn inner-read-object
  [env language datatype content]
  (cond
    (string? language)
    {:ol content :ln language}

    (string? datatype)
    (case datatype
      "https://knotation.org/datatype/link"
      (ln/object->node env content)

      ; TODO: reimplement
      ;"https://knotation.org/datatype/omn"
      ;(assoc
      ; (om/read-class-expression env content)
      ; :di datatype)

      ; TODO: warn on unrecognized Knotation datatype
      ;(string/starts-with? datatype "https://knotation.org/datatype/")

      {:ol content :di datatype})

    :else
    {:ol content}))

(defn read-object
  "Read the object part of a statement, with its language or datatype,
   and using the default language for its predicate."
  [env parse predicate-iri language datatype-iri]
  (inner-read-object
   env
   (or language (get-in env [::en/predicate-language predicate-iri]))
   (or datatype-iri (get-in env [::en/predicate-datatype predicate-iri]))
   (->> parse
        rest
        (filter #(contains? #{:lexical :eol} (first %)))
        (#(if (= :eol (->> % last first))
            (butlast %)
            %))
        (map second)
        string/join)))

;; A manchester expression is a comlpex, recursive expression
;; managed by instaparse
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

(defn parse-manchester
  [content]
  (let [result (manchester-parser content)]
    (when (insta/failure? result)
      (println result)
      (util/throw-exception "Manchester parser failure"))
    result))

(defn ->obj
  [subtree]
  (let [elem (first subtree)]
    (or (and (= :LABEL elem) {:ol (nth subtree 2)})
        (and (get subtree :sb) {:ob (get subtree :sb)})
        (and (map? subtree) (select-keys subtree [:ob :ol :oi]))
        (and (string? (first elem)) {:ob (first elem)})
        (util/error :invalid-object-extraction subtree))))

(declare read-manchester-expression)

(defn read-restriction
  [env parse restriction]
  (let [[_ left right] parse
        b (rdf/random-blank-node)
        left (read-manchester-expression env left)
        right (read-manchester-expression env right)]
    (concat
     [{:sb b :pi (rdf "type") :oi (owl "Restriction")}
      (merge {:sb b :pi (owl "onProperty")} (->obj left))
      (merge {:sb b :pi restriction} (->obj right))]
     (when (map? (first left)) left)
     (when (map? (first right)) right))))

(defn read-combination
  [env parse combination]
  (let [[_ left _ _ _ right] parse
        b1 (rdf/random-blank-node)
        b2 (rdf/random-blank-node)
        b3 (rdf/random-blank-node)
        left (read-manchester-expression env left)
        right (read-manchester-expression env right)]
    (concat
     [{:sb b1 :pi (rdf "type") :oi (owl "Class")}
      {:sb b1 :pi combination :ob b2}
      (merge {:sb b2 :pi (rdf "first")} (->obj left))
      {:sb b2 :pi (rdf "rest") :ob b3}
      (merge {:sb b3 :pi (rdf "first")} (->obj right))
      {:sb b3 :pi (rdf "rest") :oi (rdf "nil")}]
     (when (map? (first left)) left)
     (when (map? (first right)) right))))

(defn read-negation
  [env parse]
  (let [b (rdf/random-blank-node)
        target (read-manchester-expression env (last parse))
        ms [{:sb b :pi (rdf "type") :oi (owl "Class")}
            (merge
             {:sb b :pi (owl "complementOf")}
             (->obj target))]]
    (concat ms (when (map? (first target)) target))))

(defn read-manchester-expression
  [env parse]
  (case (first parse)
    (:MANCHESTER_EXPRESSION :OBJECT_PROPERTY_EXPRESSION) (read-manchester-expression env (second parse))
    :LABEL parse
    :CLASS_EXPRESSION (mapcat #(read-manchester-expression env %) (rest parse))
    :SOME (read-restriction env parse (owl "someValuesFrom"))
    :ONLY (read-restriction env parse (owl "allValuesFrom"))
    :CONJUNCTION (read-combination env parse (rdf "intersectionOf"))
    :DISJUNCTION (read-combination env parse (rdf "unionOf"))
    :NEGATION (read-negation env parse)
    (util/error :unsupported-manchester-form parse)))

(defn read-manchester
  [env parse]
  {:event :manchester-expression
   :zn :TODO-subject
   :expression-maps (vec (read-manchester-expression env parse))})

(defn read-statement
  [env parse]
  (let [names (->> parse rest (filter #(= :name (first %))))
        predicate-name (-> names first second)
        datatype-name (-> names second second)
        leading-at? (when datatype-name (string/starts-with? datatype-name "@"))]
    (if-let [predicate-iri (ln/->iri env predicate-name)]
      (if (or (nil? datatype-name)
              leading-at?
              (ln/->iri env datatype-name))
        (if-let [object
                 (read-object
                  env
                  parse
                  predicate-iri
                  (when leading-at?
                    (string/replace datatype-name #"^@" ""))
                  (when-not leading-at?
                    (ln/->iri env datatype-name)))]
          (assoc
           object
           :event :statement
           :pi predicate-iri)
          (util/error :unrecognized-object parse))
        (util/error :unrecognized-datatype datatype-name))
      (util/error :unrecognized-predicate predicate-name))))

(defn read-annotation
  [env parse]
  (merge (read-statement env (cons ::statement-block (drop 3 parse)))
         {:event :annotation :level (count (second (second parse)))}))

(defn render-datatype
  "Render the datatype part of a statement.
   Handles default dataypes and languages."
  [env predicate-iri {:keys [oi ob ol di ln] :as object}]
  (let [di (if oi "https://knotation.org/datatype/link" di)]
    (cond
      (and ln (not= ln (get-in env [::en/predicate-language predicate-iri])))
      [[:symbol ";"]
       [:space " "]
       [:name (str "@" ln)]]
      (and di (not= di (get-in env [::en/predicate-datatype predicate-iri])))
      [[:symbol ";"]
       [:space " "]
       [:name (ln/iri->name env di)]]
      :else
      [])))

(defn render-object
  "Render the lexical part of a statement."
  [env {:keys [oi ob ol di ln] :as object}]
  (cond
    oi
    [[:space " "]
     [:lexical (ln/iri->name env oi)]
     [:eol "\n"]]

    ob
    [[:space " "]
     [:lexical ob]
     [:eol "\n"]]

    ol
    (->> (util/split-lines ol)
         (mapcat (fn [line] [[:space " "] [:lexical line] [:eol "\n"]])))

    :else
    (util/error :not-an-object object)))

(defn render-statement
  [env {:keys [pi] :as state}]
  (if pi
    (if-let [predicate-name (ln/iri->name env pi)]
      (concat
       [::statement-block
        [:name predicate-name]]
       (render-datatype env pi state)
       [[:symbol ":"]]
       (render-object env state))
      (util/error :invalid-iri pi))
    (util/error :not-a-statement-state state)))

(defn render-annotation
  [env state]
  (->> state :input :parse flatten (filter string?)))

; Handle multi-line structures

(defn merge-parses
  "Given a sequence of parses, return a single parse.
   This is used to merge ::statement-line and ::indented-line
   into a single ::statement-block."
  [parses]
  ; TODO: check more carefully
  (if (contains? #{::statement-line ::annotation-line} (ffirst parses))
    (concat
     [(if (= (ffirst parses) ::statement-line) ::statement-block ::annotation-block)]
     (->> parses first rest)
     (->> parses rest (mapcat rest)))
    (first parses)))

(defn process-annotation
  [prev s]
  (if (= (:event s) :annotation)
    (let [tgt (select-keys prev [:rt :gi :si :sb :pi :oi :ob :ol :di :ln])
          ann-prev? (= (:event prev) :annotation)]
      (cond (and (not ann-prev?) (> (:level s) 1))
            (util/error :invalid-annotation-level (->> s :input :parse))

            (= (:event prev) :statement)
            (assoc s :target tgt :stack {(:level s) tgt})

            (and ann-prev? (= (:level prev) (:level s)))
            (assoc s :target (:target prev) :stack (:stack prev))

            (and ann-prev? (= (:level prev) (- (:level s) 1)))
            (assoc s :target tgt :stack (assoc (:stack prev) (:level s) tgt))

            (and ann-prev? (get (:stack prev) (:level s)))
            (assoc s :target (get (:stack prev) (:level s)) :stack (:stack prev))

            :else
            (util/error :no-annotation-target (->> s :input :parse))))
    s))

(defn process-annotations
  [states]
  ((fn rec [ss]
     (when (not (empty? (rest ss)))
       (let [res (process-annotation (first ss) (second ss))]
         (lazy-seq (cons res (rec (cons res (rest (rest ss)))))))))
   (cons nil states)))

; Primary interface: step-by-step

(defn parse-line
  "Given a line (string) in Knotation format,
   return a parse vector."
  [line]
  (case (first line)
    (nil \n) (parse-blank line)
    \# (parse-comment line)
    \@ (parse-declaration line)
    \: (parse-subject line)
    \space (parse-indented line)
    \> (parse-annotation line)
    (parse-statement line)))

(defn process-parses
  "Given a sequence of parses, return a lazy sequence of processed parses."
  [parses]
  (->> parses
       (util/partition-with #(not= ::indented-line (first %)))
       (map merge-parses)))

(defn read-parse
  "Given an environment and a parse,
   return the resulting state."
  [env parse]
  (merge
   (select-keys env [:gi :si :sb])
   (case (first parse)
     ::blank-line (read-blank env parse)
     ::comment-line (read-comment env parse)
     ::prefix-line (read-prefix env parse)
     ::subject-line (read-subject env parse)
     ::statement-block (read-statement env parse)
     ::annotation-block (read-annotation env parse)
     (util/throw-exception :bad-parse parse))))

(defn expand-state
  "Given an environment and a state,
   expand any templates using string substitution,
   returning the pair of the updated state and a sequence of new parses
   (empty if this is not a template statement)."
  [env {:keys [pi ol] :as state}]
  (if (= pi "https://knotation.org/predicate/apply-template")
    (handler-case
     (let [template-iri (->> ol
                             util/split-lines
                             first
                             string/trim
                             (ln/->iri env))
           values (->> ol
                       util/split-lines
                       rest
                       (map #(string/split % #": " 2))
                       (into {}))]
       [(assoc state :pi "https://knotation.org/predicate/applied-template")
        (->> (string/replace
              (get-in env [::en/template-content template-iri])
              #"\{(.*?)\}"
              (fn [[_ x]] (get values x)))
             util/split-lines
             rest
             (map parse-line)
             process-parses)])
     (:default e (util/throw-exception "Template error for " ol " " e)))
    [state []]))

(defn render-state
  [env {:keys [:event] :as state}]
  (case event
    nil nil
    :blank (render-blank env state)
    :comment (render-comment env state)
    :prefix (render-prefix env state)
    :graph-start [::graph-start]
    :graph-end [::graph-end]
    :subject-start (render-subject env state)
    :subject-end [::subject-end]
    :statement (render-statement env state)
    :annotation (render-annotation env state)
    (util/throw-exception :bad-state state)))

; Implement format interface

(defmethod fm/parse-line
  :kn
  [fmt line]
  (parse-line line))

(defmethod fm/process-parses
  :kn
  [fmt parses]
  (process-parses parses))

(defmethod fm/process-states
  :kn
  [fmt states]
  (process-annotations states))

(defmethod fm/read-parse
  :kn
  [fmt env parse]
  (read-parse env parse))

(defmethod fm/expand-state
  :kn
  [fmt env state]
  (expand-state env state))

(defmethod fm/render-state
  :kn
  [fmt env state]
  (render-state env state))
