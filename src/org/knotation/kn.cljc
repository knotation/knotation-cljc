(ns org.knotation.kn
  (:require [clojure.string :as string]
            [org.knotation.util :as util]
            [instaparse.core :as insta]
            [org.knotation.util :as util]
            [org.knotation.rdf :as rdf :refer [owl rdf kn]]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]
            [org.knotation.omn :as omn]))

(defn parse-map
  "Transform a parse vector into a keyword-string map.
   Beware of duplicate keys!"
  [parse]
  (->> parse
       rest
       (map (fn [[k v & xs]] [k v]))
       (into {})))

; A blank line contains nothing except an optional line ending.

(defn parse-blank
  "Given a blank line (string), return a parse."
  [line]
  (if (re-matches #"\n?" line)
    [::blank-line
     [:eol "\n"]]
    (util/error :not-a-blank-line line)))

(defn read-blank
  "Given a state with ::st/parse for a blank line, return a state."
  [state]
  (assoc state ::st/event ::st/blank))

(defn render-blank
  "Given a ::st/blank state, return a parse."
  [state]
  [::blank-line
   [:eol "\n"]])

; A comment is a line starting with '#'.

(defn parse-comment
  "Given a comment line (string), return a parse."
  [line]
  (if-let [[_ comment] (re-matches #"#(.*)\n?" line)]
    [::comment-line
     [:symbol "#"]
     [:comment comment]
     [:eol "\n"]]
    (util/error :not-a-comment-line line)))

(defn read-comment
  "Given a state with ::st/parse for a comment line, return a state."
  [{:keys [::st/parse] :as state}]
  (if-let [comment (-> parse parse-map :comment)]
    (assoc
     state
     ::st/event ::st/comment
     :comment comment)
    (st/error state :not-a-comment-parse)))

(defn render-comment
  "Given a ::st/comment state, return a parse."
  [{:keys [comment] :as state}]
  (if comment
    [::comment-line
     [:symbol "#"]
     [:comment comment]
     [:eol "\n"]]
    (st/error state :not-a-comment-state)))

; A prefix declaration is a line starting with '@prefix '

(defn parse-prefix
  "Given a prefix line (string), return a parse."
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
  "Given a state with ::st/parse for a prefix line, return a state."
  [{:keys [::st/parse] :as state}]
  (let [{:keys [keyword prefix iri]} (parse-map parse)]
    (if (and (= keyword "prefix") prefix iri)
      (assoc
       state
       ::st/event ::st/prefix
       ::en/prefix prefix
       ::en/iri iri)
      (st/error state :not-a-prefix-parse parse))))

(defn render-prefix
  "Given a ::st/prefix state, return a parse."
  [{:keys [::en/prefix ::en/iri] :as state}]
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
    (st/error state :not-a-prefix-state)))

; A declaration is one of: prefix, (TODO: base, graph)

(defn parse-declaration
  "Given a decalaration line (string), return a parse."
  [line]
  (cond
    (string/starts-with? line "@prefix ")
    (parse-prefix line)
    :else
    (util/error :unrecognized-declaration-line line)))

; A subject line start with ': ' followed by the name of the subjects

(defn parse-subject
  "Given a subject line (string), return a parse."
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
  "Given a state with ::st/parse for a subject line, return a state."
  [{:keys [::en/env ::st/parse] :or {env en/default-env} :as state}]
  (if-let [name (-> parse parse-map :name)]
    (if-let [iri (en/name->iri env name)]
      (assoc
       state
       ::st/event ::st/subject-start
       ::rdf/stanza iri
       ::rdf/subject iri)
      (st/error state :unrecognized-name name))
    (st/error state :not-a-subject-parse)))

(defn render-subject
  "Given a ::st/subject-start state, return a parse."
  [{:keys [::en/env ::rdf/subject] :or {env en/default-env} :as state}]
  (if subject
    (if-let [name (or (and (rdf/blank? subject) subject)
                      (en/iri->name env subject))]
      [::subject-line
       [:symbol ":"]
       [:space " "]
       [:name name]
       [:eol "\n"]]
      (st/error state :invalid-iri name))
    (st/error state :not-a-subject-state)))

; Indented lines continue the content of the previous statement.
; They must be merged into the previous statement-line
; to create a statement-block.

(defn parse-indented
  "Given an indented line (string), return a parse."
  [line]
  (if-let [[_ lexical] (re-matches #" (.*)\n?" line)]
    [::indented-line
     [:space " "]
     [:lexical lexical]
     [:eol "\n"]]
    (util/error :not-an-indented-line line)))

(defn merge-indented-statement
  "Given a sequence of states, return a single state.
   This is used to merge ::statement-line and ::indented-line
   into a single ::statement-block."
  [states]
  (if (-> states first ::st/parse first (= ::statement-line))
    (-> states
        first
        (assoc-in [::st/input ::st/content]
                  (->> states
                       (map ::st/input)
                       (map ::st/content)
                       (string/join "\n")))
        (assoc ::st/parse
               (concat
                [::statement-block]
                (->> states first ::st/parse rest)
                (->> states rest (map ::st/parse) (mapcat rest)))))
    (first states)))

(defn merge-indented
  "Given a sequence of states,
   merge indented lines into blocks
   and return a sequence of states."
  [states]
  (->> states
       (util/partition-with #(-> % ::st/parse first (not= ::indented-line)))
       (map merge-indented-statement)))

; Statements are the most complicated,
; since they can expand to multiple states.

(defn parse-statement
  "Given a statement line (string), return a parse."
  [line]
  (if-let [[_ arrows pd lexical] (re-matches #"(>* )?(.*?): (.*)\n?" line)]
    (if-let [[predicate datatype] (string/split pd #"; " 2)]
      (if datatype
        [::statement-line
         [:arrows (or arrows "")]
         [:name predicate]
         [:symbol ";"]
         [:space " "]
         [:name datatype]
         [:symbol ":"]
         [:space " "]
         [:lexical lexical]
         [:eol "\n"]]
        [::statement-line
         [:arrows (or arrows "")]
         [:name predicate]
         [:symbol ":"]
         [:space " "]
         [:lexical lexical]
         [:eol "\n"]])
      (util/error :invalid-predicate-datatype pd))
    (util/error :not-a-statement line)))

(defn inner-read-object
  "Given an environment, language tag (or nil), datatype IRI (or nil),
   and content string,
   return the object part of an RDF quad."
  [env language datatype content]
  (cond
    (string? language)
    #::rdf{:ol content :lt language}

    (string? datatype)
    (case datatype
      "https://knotation.org/kn/link"
      (if (rdf/blank? content)
        {::rdf/ob content}
        {::rdf/oi (en/name->iri env content)})

      "https://knotation.org/kn/omn"
      (let [res (omn/read-class-string env content)]
        (merge
         {:states (map #(assoc % ::rdf/di datatype) res)
          ::rdf/di datatype}
         (omn/->obj env res)))

      ; TODO: warn on unrecognized Knotation datatype
      ;(string/starts-with? datatype "https://knotation.org/kn/")

      #::rdf{:ol content :di datatype})

    :else #::rdf{:ol content}))

(defn read-object
  "Read the object part of a statement, with its language or datatype,
   and using the default language for its predicate,
   and return the object part of an RDF quad."
  [env parse predicate-iri language datatype-iri]
  (inner-read-object
   env
   (or language (en/get-language env predicate-iri))
   (or datatype-iri (en/get-datatype env predicate-iri))
   (->> parse
        rest
        (filter #(contains? #{:lexical :eol} (first %)))
        (#(if (= :eol (->> % last first))
            (butlast %)
            %))
        (map second)
        string/join)))

(defn read-statement
  "Given a state with a ::st/parse for a statement,
   return a sequence of states for the statement, any expanded templates,
   and any anonymous structures such as OWL annotations, RDF lists, and OWL logic."
  [{:keys [::en/env ::st/parse ::rdf/stanza ::rdf/subject] :as state}]
  (let [names (->> parse rest (filter #(= :name (first %))))
        predicate-name (-> names first second)
        datatype-name (-> names second second)
        leading-at? (when datatype-name (string/starts-with? datatype-name "@"))]
    (if-let [predicate-iri (when predicate-name (en/name->iri env predicate-name))]
      (if (or (nil? datatype-name)
              leading-at?
              (when datatype-name (en/name->iri env datatype-name)))
        (if-let [object
                 (read-object
                  env
                  parse
                  predicate-iri
                  (when leading-at?
                    (string/replace datatype-name #"^@" ""))
                  (when-not leading-at?
                    (when datatype-name (en/name->iri env datatype-name))))]
          [(assoc
            state
            ::st/event ::st/statement
            ::rdf/quad
            (merge
             object
             (if (rdf/blank? subject) {::rdf/sb subject} {::rdf/si subject})
             {::rdf/zn stanza
              ::rdf/pi predicate-iri}))]
          [(st/error state :unrecognized-object parse)])
        [(st/error state :unrecognized-datatype datatype-name)])
      [(st/error state :unrecognized-predicate predicate-name)])))

(defn render-datatype
  "Render the datatype part of a statement.
   Handles default dataypes and languages."
  [env predicate-iri {::rdf/keys [oi ob ol di lt] :as object}]
  (let [di (if (or oi ob) "https://knotation.org/kn/link" di)]
    (cond
      (and lt (not= lt (en/get-language env predicate-iri)))
      [[:symbol ";"]
       [:space " "]
       [:name (str "@" lt)]]
      (= di (rdf/xsd "string")) ; ignore xsd:string
      []
      (and di (not= di (en/get-datatype env predicate-iri)))
      [[:symbol ";"]
       [:space " "]
       [:name (en/iri->name env di)]]
      :else
      [])))

(defn render-object
  "Render the lexical part of a statement."
  [env {::rdf/keys [oi ob ol di lt] :as object}]
  (cond
    oi
    [[:space " "]
     [:lexical (en/iri->name env oi)]
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
  "Given a ::st/statement state, return a parse."
  [{:keys [::en/env ::rdf/quad] :as state}]
  (if-let [pi (::rdf/pi quad)]
    (if-let [predicate-name (en/iri->name env pi)]
      (concat
       [::statement-block
        [:name predicate-name]]
       (render-datatype env pi quad)
       [[:symbol ":"]]
       (render-object env quad))
      (st/error state :invalid-predicate-iri pi))
    (st/error state :not-a-statement-state)))

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

(defn parse-lines
  "Given a sequence of lines,
   return a sequence of states with ::st/input and ::st/parse."
  [lines]
  (->> lines
       (map-indexed
        (fn [i line]
          (-> {::st/location {::st/line-number (inc i) ::st/column-number 1}}
              (st/input :kn line)
              (assoc ::st/event ::st/parse ::st/parse (parse-line line)))))
       merge-indented))

(defn read-parse
  "Given a state with a ::st/parse,
   read it, expand it as required,
   and return a sequence of resulting states."
  [state]
  (case (-> state ::st/parse first)
    ::blank-line [(read-blank state)]
    ::comment-line [(read-comment state)]
    ::prefix-line [(read-prefix state)]
    ::subject-line [(read-subject state)]
    ::statement-block (read-statement state)
    (util/throw-exception :bad-parse state)))

(defn read-parses
  "Given an initial state and a sequence of states with ::st/parse,
   read the parses and return the fully processed states."
  [initial-state states]
  (->> states
       (reductions
        (fn [previous-states state]
          (->> state
               (st/update-state (last previous-states))
               read-parse))
        [initial-state])
       rest
       (mapcat identity)))

(defn read-lines
  "Given a initial state and a sequence of lines (strings)
   return a sequence of states."
  [initial-state lines]
  (->> lines
       parse-lines
       (read-parses initial-state)))

(defn read-input
  "Given an initial state and a string,
   return a sequence of states."
  [initial-state input]
  (->> input
       util/split-lines
       (read-lines initial-state)))

(defn render-state
  "Given a state, render it and return the updated state."
  [{:keys [::st/event] :as state}]
  (->> (case event
         ::st/blank (render-blank state)
         ::st/comment (render-comment state)
         ::st/prefix (render-prefix state)
         ::st/graph-start state ; TODO
         ::st/graph-end state
         ::st/stanza-start state
         ::st/stanza-end state
         ::st/subject-start (render-subject state)
         ::st/subject-end state
         ::st/statement (render-statement state)
         (util/throw-exception :bad-state state))
       st/render-parse
       (st/output state :kn)))

(defn render-states
  "Given an initial environment and a sequence of states,
   return a sequence of states with ::st/output."
  [env states]
  (->> states
       (reductions
        (fn [previous-state state]
          (->> state
               (st/update-state previous-state)
               render-state))
        {::en/env env :line-number 1 :column-number 1})
       rest))
