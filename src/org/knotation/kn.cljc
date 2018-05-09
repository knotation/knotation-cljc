(ns org.knotation.kn
  (:require [clojure.string :as string]
            [org.knotation.util :as util]
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
    (util/starts-with? line "@prefix ")
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
    (if-let [iri (ln/subject->iri env name)]
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
      ; :dt datatype)

      ; TODO: warn on unrecognized Knotation datatype
      ;(util/starts-with? datatype "https://knotation.org/datatype/")

      {:ol content :dt datatype})

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

(defn read-statement
  [env parse]
  (let [names (->> parse rest (filter #(= :name (first %))))
        predicate-name (-> names first second)
        datatype-name (-> names second second)]
    (if-let [predicate-iri (ln/predicate->iri env predicate-name)]
      (if (or (nil? datatype-name)
              (util/starts-with? datatype-name "@")
              (ln/datatype->iri env datatype-name))
        (if-let [object
                 (read-object
                  env
                  parse
                  predicate-iri
                  (when (util/starts-with? datatype-name "@")
                    (string/replace datatype-name #"^@" ""))
                  (when-not (util/starts-with? datatype-name "@")
                    (ln/datatype->iri env datatype-name)))]
          (assoc
           object
           :event :statement
           :pi predicate-iri)
          (util/error :unrecognized-object parse))
        (util/error :unrecognized-datatype datatype-name))
      (util/error :unrecognized-predicate predicate-name))))

(defn render-datatype
  "Render the datatype part of a statement.
   Handles default dataypes and languages."
  [env predicate-iri {:keys [oi ob ol dt ln] :as object}]
  (let [dt (if oi "https://knotation.org/datatype/link" dt)]
    (cond
      (and ln (not= ln (get-in env [::en/predicate-language predicate-iri])))
      [[:symbol ";"]
       [:space " "]
       [:name (str "@" ln)]]
      (and dt (not= dt (get-in env [::en/predicate-datatype predicate-iri])))
      [[:symbol ";"]
       [:space " "]
       [:name (ln/iri->name env dt)]]
      :else
      [])))

(defn render-object
  "Render the lexical part of a statement."
  [env {:keys [oi ob ol dt ln] :as object}]
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
    (->> (string/split ol #"\n" 0)
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

; Handle multi-line structures

(defn merge-parses
  "Given a sequence of parses, return a single parse.
   This is used to merge ::statement-line and ::indented-line
   into a single ::statement-block."
  [parses]
  ; TODO: check more carefully
  (if (= ::statement-line (ffirst parses))
    (concat
     [::statement-block]
     (->> parses first rest)
     (->> parses rest (mapcat rest)))
    (first parses)))

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
    (parse-statement line)))

(defn process-parses
  "Given a sequence of parses, return a lazy sequence of merged parses."
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
     (util/throw-exception :bad-parse parse))))

(defn expand-state
  "Given an environment and a state,
   expand any templates using string substitution,
   returning the pair of the updated state and a sequence of new parses
   (empty if this is not a template statement)."
  [env {:keys [pi ol] :as state}]
  (if (= pi "https://knotation.org/predicate/apply-template")
    (let [template-iri (->> ol
                            util/split-lines
                            first
                            string/trim
                            (ln/predicate->iri env))
          values (->> ol
                      util/split-lines
                      rest
                      (map #(string/split % #": "))
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
