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

(declare parse-lines)
(declare read-parse)
(declare read-lines)

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

; A subject line start with ': ' followed by the name of the subject

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
     [:indent " "]
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
  (if-let [[_ arrows pd space lexical]
           (or (re-matches #"(>* )?(.*?):()()\n?" line)
               (re-matches #"(>* )?(.*?):( )(.*)\n?" line))]
    (if-let [[predicate datatype] (string/split pd #"; " 2)]
      (if datatype
        [::statement-line
         [:arrows (or arrows "")]
         [:name predicate]
         [:symbol ";"]
         [:space " "]
         [:name datatype]
         [:symbol ":"]
         [:space space]
         [:lexical lexical]
         [:eol "\n"]]
        [::statement-line
         [:arrows (or arrows "")]
         [:name predicate]
         [:symbol ":"]
         [:space space]
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
      "https://knotation.org/kn/anon"
      {::rdf/ob (rdf/random-blank-node)
       ::rdf/di datatype}

      "https://knotation.org/kn/link"
      (if (rdf/blank? content)
        {::rdf/ob content}
        {::rdf/oi (en/name->iri env content)})

      "https://knotation.org/kn/omn"
      {::rdf/ob (rdf/random-blank-node)
       ::rdf/di datatype}

      ; TODO: warn on unrecognized Knotation datatype
      ;(string/starts-with? datatype "https://knotation.org/kn/")

      #::rdf{:ol content :di datatype})

    :else #::rdf{:ol content}))

(defn read-content
  "Given a parse, return a content string."
  [parse]
  (->> parse
       rest
       (filter #(contains? #{:lexical :eol} (first %)))
       (#(if (= :eol (->> % last first))
           (butlast %)
           %))
       (map second)
       string/join))

(defn read-object
  "Read the object part of a statement, with its language or datatype,
   and using the default language for its predicate,
   and return the object part of an RDF quad."
  [env parse predicate-iri language datatype-iri]
  (inner-read-object
   env
   (or language (en/get-language env predicate-iri))
   (or datatype-iri (en/get-datatype env predicate-iri))
   (read-content parse)))

(defn expand-anonymous
  "Given a state, return a sequence of states.
   If this is an anonymous subject,
   that means recursively reading the content."
  [{:keys [::st/parse ::en/env ::rdf/quad ::rdf/stanza] :as state}]
  (cond
    (= (::rdf/di quad) "https://knotation.org/kn/anon")
    (->> parse
         read-content
         util/split-lines
         rest
         (read-lines
          (assoc state
                 ::rdf/subject (::rdf/ob quad)
                 ::rdf/quad (dissoc quad ::rdf/di)))
         (concat [state]))

    (= (::rdf/di quad) "https://knotation.org/kn/list")
    (->> (string/split (read-content parse) #"\n" -1)
         rest
         (map
          (fn [line]
            (case (first line)
              \~ (str (rdf/rdf "first") "; " (subs line 2))
              \- (str (rdf/rdf "first") ": " (subs line 2))
              line)))
         (parse-lines state)
         (map #(read-parse (st/update-state state %)))
         (#(concat % [nil]))
         (reduce
          (fn [previous-states current-states]
            (let [quad (::rdf/quad (first current-states))
                  sb (-> previous-states last last ::rdf/quad ::rdf/ob)]
              (if current-states
                (conj
                 previous-states
                 (concat
                  ; update rdf:first
                  [(assoc
                    (first current-states)
                    ::rdf/subject sb
                    ::rdf/quad
                    (-> current-states
                        first
                        ::rdf/quad
                        (dissoc ::rdf/si)
                        (assoc ::rdf/sb sb)))]
                  ; add other states
                  (rest current-states)
                  ; add rdf:rest
                  [(assoc
                    state
                    ::rdf/subject sb
                    ::rdf/quad
                    {::rdf/zn stanza
                     ::rdf/sb sb
                     ::rdf/pi (rdf/rdf "rest")
                     ::rdf/ob (rdf/random-blank-node)})]))
                ; update rdf:nil
                (conj
                 (vec (butlast previous-states))
                 (conj
                  (vec (butlast (last previous-states)))
                  (-> previous-states
                      last
                      last
                      (update-in [::rdf/quad] dissoc ::rdf/ob)
                      (assoc-in [::rdf/quad ::rdf/oi] (rdf/rdf "nil"))))))))
          ; start with first statement, pointing to a random blank node
          [[(assoc
             state
             ::rdf/quad
             (-> quad
                 (select-keys [::rdf/zn ::rdf/si ::rdf/sb ::rdf/pi])
                 (assoc ::rdf/ob (rdf/random-blank-node))))]])
         (mapcat identity))

    (= (rdf/kn "omn") (::rdf/di quad))
    (let [quad (dissoc quad ::rdf/di)
          res (omn/read-class-string env (read-content parse))]
      (->> res
           (map #(assoc % ::rdf/zn stanza))
           (concat [(assoc quad ::rdf/ob (-> res first ::rdf/sb))])
           (map #(assoc state ::rdf/stanza stanza ::rdf/quad %))
           (map #(assoc % ::rdf/subject (st/get-subject %)))))

    :else
    [state]))

(defn expand-annotation
  "Given a state for a statement, expand OWL annotations ('> ')
   and return a sequence of states.
   Annotations are expanded by generating five statements."
  [{:keys [::st/parse ::rdf/stanza ::rdf/quad ::st/quad-stack] :as state}]
  (let [arrows (->> parse rest (filter #(= :arrows (first %))) first second)
        depth (if arrows (-> arrows string/trim count) 0)]
    (cond
      (= 0 depth)
      [(assoc state ::st/quad-stack [quad])]

      (> depth (count quad-stack))
      [(st/error state :invalid-annotation-depth arrows)]

      :else
      (let [{::rdf/keys [si sb pi] :as target} (get quad-stack (dec depth))
            bn (rdf/random-blank-node)
            quad (-> quad (dissoc ::rdf/si) (assoc ::rdf/sb bn))
            state (assoc state ::st/quad-stack (vec (concat (take depth quad-stack) [quad])))]
        (->> [{::rdf/pi (rdf/rdf "type") ::rdf/oi (rdf/owl "Annotation")}
              (merge
               {::rdf/pi (rdf/owl "annotatedSource")}
               (when si {::rdf/oi si})
               (when sb {::rdf/ob sb}))
              {::rdf/pi (rdf/owl "annotatedProperty") ::rdf/oi pi}
              (merge
               (dissoc target ::rdf/si ::rdf/sb)
               {::rdf/pi (rdf/owl "annotatedTarget")})
              quad]
             (map #(assoc % ::rdf/zn stanza ::rdf/sb bn))
             (map #(assoc state ::rdf/subject bn ::rdf/quad %)))))))

(defn expand-template
  "Given a state for a statement, return a sequence of states.
   If the predicate is kn:apply-template
   then use string substitution to fill in the template,
   and read the template as a sequence of states."
  [{:keys [::en/env ::rdf/quad] :as state}]
  (if (= (::rdf/pi quad) "https://knotation.org/kn/apply-template")
    (let [value (::rdf/ol quad)
          template-iri (->> value
                            string/split-lines
                            first
                            string/trim
                            (en/name->iri env))
          value-map (->> value
                         string/split-lines
                         rest
                         (map #(string/split % #": "))
                         (into {}))]
      (if-let [content (en/get-template-content env template-iri)]
        (->> (string/replace
              content
              #"\{(.*?)\}"
              (fn [[_ x]] (get value-map x "UNKNOWN")))
             util/split-lines
             rest
             (read-lines state)
             (concat
              [(assoc-in state [::rdf/quad ::rdf/pi] (rdf/kn "applied-template"))]))
        [(st/error state :unrecognized-template template-iri)]))
    [state]))

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
          (->> object
               (merge
                (if (rdf/blank? subject) {::rdf/sb subject} {::rdf/si subject})
                {::rdf/zn stanza
                 ::rdf/pi predicate-iri})
               (assoc state ::st/event ::st/statement ::rdf/quad)
               expand-anonymous
               (mapcat expand-annotation)
               (mapcat expand-template)
               ; append a ::st/next-subject key
               ((fn [states] (concat (butlast states) [(assoc (last states) ::st/next-subject subject)]))))

          [(st/error state :unrecognized-object parse)])
        [(st/error state :unrecognized-datatype datatype-name)])
      [(st/error state :unrecognized-predicate predicate-name)])))

(defn render-datatype?
  "True if this datatype should be rendered, otherwise false."
  [env predicate-iri {::rdf/keys [oi ob ol di lt] :as object}]
  (let [di (if (and (nil? di) (or oi ob)) "https://knotation.org/kn/link" di)]
    (cond
      (and di (= di (rdf/xsd "string"))) false ; ignore xsd:string
      (and lt (not= lt (en/get-language env predicate-iri))) true ; default language
      (and di (not= di (en/get-datatype env predicate-iri))) true ; default datatype
      :else false)))

(defn render-datatype
  "Render the datatype part of a statement.
   Handles default dataypes and languages."
  [env predicate-iri {::rdf/keys [oi ob ol di lt] :as object}]
  (cond
    (not (render-datatype? env predicate-iri object))
    []

    lt
    [:name (str "@" lt)]

    di
    [:name (en/iri->name env di)]

    (or oi ob)
    [:name (en/iri->name env (rdf/kn "link"))]))

(defn render-object
  "Render the lexical part of a statement."
  [env {::rdf/keys [oi ob ol di lt] :as object}]
  (cond
    (= (rdf/kn "omn") di)
    [[:space " "]]

    (contains? #{(rdf/kn "anon") (rdf/kn "list")} di)
    [[:eol "\n"]]

    oi
    [[:space " "]
     [:lexical (en/iri->name env oi)]
     [:eol "\n"]]

    ob
    [[:space " "]
     [:lexical ob]
     [:eol "\n"]]

    ol
    (let [lines (string/split ol #"\n" -1) ; NOTE: NOT util/split-lines
          line (first lines)]
      (concat
       (if (= line "")
         [[:eol "\n"]]
         [[:space " "]
          [:lexical line]
          [:eol "\n"]])
       (->> lines
            rest
            (mapcat
             (fn [line]
               [[:space " "]
                [:lexical line]
                [:eol "\n"]])))))

    :else
    (util/error :not-an-object object)))

(defn render-statement
  "Given a ::st/statement state, return a parse."
  [{:keys [::list-item? ::annotation ::en/env ::rdf/quad ::depth ::exact ::omn ::before ::after] :as state}]
  (if-let [pi (::rdf/pi quad)]
    (if-let [predicate-name (en/iri->name env pi)]
      (cond
        exact
        exact

        omn
        (let [name (en/iri->name env (::rdf/oi quad))]
          (concat
           before
           (if (re-find #"\s" name)
             [[:symbol "'"] [:lexical name] [:symbol "'"]]
             [[:lexical name]])
           after))

        :else
        (concat
         before

         ; Handle indentation
         (cond
           annotation
           [::statement-block
            [:arrows (apply str (concat (repeat depth ">") [" "]))]]
           (and depth (> depth 0))
           [::indented-line
            [:indent (apply str (concat (repeat depth " ")))]]
           :else
           [::statement-block])

         ; Handle predicate or list item
         (cond
           (and list-item? (render-datatype? env pi quad))
           [[:symbol "~"]
            [:space " "]]
           list-item?
           [[:symbol "-"]]
           :else
           [[:name predicate-name]])

         ; Handle datatype
         (if (render-datatype? env pi quad)
           (concat
            (when-not list-item?
              [[:symbol ";"]
               [:space " "]])
            (render-datatype env pi quad)))

         (when-not (and list-item? (not (render-datatype? env pi quad)))
           [[:symbol ":"]])

         ; Handle object
         (render-object env quad)

         after))
      (st/error state :invalid-predicate-iri pi))
    (st/error state :not-a-statement-state)))

; Primary interface: step-by-step

(defn parse-line
  "Given a line (string) in Knotation format,
   return a parse vector."
  [line]
  (case (first line)
    (nil \newline) (parse-blank line)
    \# (parse-comment line)
    \@ (parse-declaration line)
    \: (parse-subject line)
    \space (parse-indented line)
    (parse-statement line)))

(defn parse-lines
  "Given a previous state and a sequence of lines,
   return a sequence of states with ::st/input and ::st/parse."
  [previous-state lines]
  (->> lines
       (reductions
        (fn [previous-state line]
          (-> previous-state
              (select-keys [::st/event ::st/location])
              (st/input :kn line)
              (assoc ::st/event ::st/parse ::st/parse (parse-line line))))
        previous-state)
       rest
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
       (parse-lines initial-state)
       (read-parses initial-state)
       st/insert-subject-events
       st/insert-stanza-events))

(defn read-input
  "Given an initial state and a string,
   return a sequence of states."
  [initial-state input]
  (->> input
       util/split-lines
       (read-lines initial-state)))

(defn render-state
  "Given a state, render it and return the updated state."
  [{:keys [::silent ::st/event] :as state}]
  (if silent
    state
    (->> (case event
           ::st/blank (render-blank state)
           ::st/comment (render-comment state)
           ::st/prefix (render-prefix state)
           ::st/graph-start [] ; TODO
           ::st/graph-end []
           ::st/stanza-start []
           ::st/stanza-end []
           ::st/subject-start (render-subject state)
           ::st/subject-end []
           ::st/statement (render-statement state)
           (util/throw-exception :bad-state state))
         st/render-parse
         (st/output state :kn))))

(defn annotate-annotation
  "Given a state for an annotation,
   return a state, maybe with a ::silent key."
  [{:keys [::rdf/quad] :as state}]
  (let [state (assoc state ::annotation true)]
    (if quad
      (if (contains? #{(rdf/rdf "type")
                       (rdf/owl "annotatedSource")
                       (rdf/owl "annotatedProperty")
                       (rdf/owl "annotatedTarget")}
                     (::rdf/pi quad))
        (assoc state ::silent true)
        state)
      (assoc state ::silent true))))

(declare omn-sort-statements)

(defn omn-sort-list
  [coll]
  (loop [{:keys [::subjects] :as coll} coll]
    (let [subject (first subjects)
          states (get coll subject)
          state (first states)
          quad (::rdf/quad state)
          pi (::rdf/pi quad)]
      (if (and state (contains? #{(rdf/rdf "first") (rdf/rdf "rest")} pi))
        (recur
         (let [state (if (= (rdf/rdf "rest") pi)
                       (if (::rdf/ob quad)
                         (assoc state ::exact [[:space " "]
                                               [:keyword "and"]
                                               [:space " "]])
                         (assoc state ::silent true))
                       state)
               state (if (and (= (rdf/rdf "first") pi) (::rdf/ob quad))
                       (assoc state ::silent true)
                       state)
               coll (-> coll
                        (update ::states conj (assoc state ::omn true))
                        (update subject rest))]
           (if-let [ob (when (= (rdf/rdf "rest") pi) (::rdf/ob quad))]
             (assoc coll ::subjects (concat [ob] subjects))
             (if-let [ob (when (= (rdf/rdf "first") pi) (::rdf/ob quad))]
               (-> coll
                   (assoc ::subjects (concat [ob] subjects))
                   omn-sort-statements)
               coll))))
        coll))))

(defn omn-sort-statements
  [{:keys [::subjects] :as coll}]
  (let [subject (first subjects)
        states (get coll subject)
        state (first states)
        rdf-type (->> states (map ::rdf/quad) (filter #(= (rdf/rdf "type") (::rdf/pi %))) first ::rdf/oi)]
    (cond
      (= (rdf/owl "Class") rdf-type)
      (let [rdf-type (->> states (filter #(= (rdf/rdf "type") (-> % ::rdf/quad ::rdf/pi))) first)
            intersection-of (->> states (filter #(= (rdf/rdf "intersectionOf") (-> % ::rdf/quad ::rdf/pi))) first)
            ob (-> intersection-of ::rdf/quad ::rdf/ob)]
        (if intersection-of
          (-> coll
              (update ::states conj (assoc rdf-type ::silent true))
              (update ::states conj (assoc intersection-of ::silent true))
              (update subject (partial remove #{rdf-type}))
              (update subject (partial remove #{intersection-of}))
              (assoc ::subjects (concat [ob] subjects))
              (update ::depth inc)
              omn-sort-list)
          (-> coll
              (update ::states conj (assoc rdf-type ::silent true))
              (update subject rest))))

      (= (rdf/owl "Restriction") rdf-type)
      (let [rdf-type (->> states (filter #(= (rdf/rdf "type") (-> % ::rdf/quad ::rdf/pi))) first)
            on-property (->> states (filter #(= (rdf/owl "onProperty") (-> % ::rdf/quad ::rdf/pi))) first)
            some-values (->> states (filter #(= (rdf/owl "someValuesFrom") (-> % ::rdf/quad ::rdf/pi))) first)]
        (-> coll
            (update ::states conj (assoc rdf-type ::silent true))
            (update ::states conj (assoc on-property
                                         ::before [[:symbol "("]]
                                         ::omn true))
            (update ::states conj (assoc some-values
                                         ::before [[:space " "]
                                                   [:keyword "some"]
                                                   [:space " "]]
                                         ::omn true
                                         ::after [[:symbol ")"]]))
            (update subject (partial remove #{rdf-type}))
            (update subject (partial remove #{on-property}))
            (update subject (partial remove #{some-values}))))

      :else
      coll)))

(defn inner-sort-statements
  "Given a map from subjects to sequences of their states,
   plus :subjects and ::states sequences,
   a :lists map and a ::depth integer,
   recursively loop through the :subjects and add to :states,
   in the order and depth that Turtle expects."
  [coll]
  (loop [{:keys [::subjects ::annotations ::quad-annotations ::depth ::subject-depth] :as coll} coll]
    (if-let [subject (first subjects)]
      (recur
       (let [coll (if (find subject-depth subject)
                    coll
                    (assoc-in coll [::subject-depth subject] depth))
             depth (get-in coll [::subject-depth subject])
             coll (assoc coll ::depth depth)]
         (if-let [state (first (get coll subject))]
           (let [state (assoc state ::depth depth)
                 state (if (contains? annotations subject) (annotate-annotation state) state)
                 quad (::rdf/quad state)
                 pi (::rdf/pi quad)
                 list-item? (contains? #{(rdf/rdf "first") (rdf/rdf "rest")} pi)
                 state (if list-item? (assoc state ::list-item? true) state)
                 state (if (= (rdf/rdf "rest") pi) (assoc state ::silent true) state)
                 anns (get quad-annotations (::rdf/quad state))
                 ob (::rdf/ob quad)
                 anon-ob? (and ob (contains? (set subjects) ob))
                 state (if anon-ob? (assoc-in state [::rdf/quad ::rdf/di] (rdf/kn "anon")) state)
                 list-ob? (and ob (get coll ob) (->> (get coll ob) (map ::rdf/quad) (map ::rdf/pi) (filter #{(rdf/rdf "first") (rdf/rdf "rest")}) first boolean))
                 state (if list-ob? (assoc-in state [::rdf/quad ::rdf/di] (rdf/kn "list")) state)
                 omn-ob? (and ob
                              (get coll ob)
                              (->> (get coll ob)
                                   (map ::rdf/quad)
                                   (filter #(and (= (rdf/rdf "type") (::rdf/pi %))
                                                 (contains? #{(rdf/owl "Class") (rdf/owl "Restriction")} (::rdf/oi %))))
                                   first
                                   boolean))
                 state (if omn-ob? (assoc-in state [::rdf/quad ::rdf/di] (rdf/kn "omn")) state)
                 coll (-> coll
                          (update ::states conj state)
                          (update subject rest))]
             (cond
               omn-ob?
               (-> coll
                   (assoc ::subjects (concat [ob] subjects))
                   omn-sort-statements
                   ; append a newline
                   ((fn [{:keys [::states] :as coll}]
                      (update-in coll [::states (dec (count states)) ::after] (fnil conj []) [:eol "\n"]))))

               ; inner list object: insert this state then switch to that subject; do not indent!
               (and list-ob? (= (rdf/rdf "rest") pi))
               (-> coll
                   (assoc ::subjects (concat [ob] subjects)))

               ; anonymous object: insert this state then switch to that subject; indent
               anon-ob?
               (-> coll
                   (assoc ::subjects (concat [ob] subjects))
                   (update ::depth inc))

               ; state with annotations: insert this state then switch to those subjects
               anns
               (-> coll
                   (assoc ::subjects (concat anns subjects))
                   (update ::depth inc))

               ; state without annotations
               :else
               coll))

           ; no more states for this subject
           (-> coll
               (dissoc subject)
               (assoc ::subjects (rest subjects))
               (assoc ::depth depth)))))

      ; no more subjects
      coll)))

(defn sort-statements
  [grouped-states annotations quad-annotations subjects]
  (-> grouped-states
      (assoc ::states []
             ::subjects subjects
             ::annotations annotations
             ::quad-annotations quad-annotations
             ::depth 0)
      inner-sort-statements
      ::states))

(defn sort-stanza
  "Given a sequence of states for one stanza,
   return them in the required order for Knotation rendering."
  [states]
  (let [zn (-> states first ::rdf/stanza)
        grouped (group-by ::rdf/subject states)
        quads (->> states (map ::rdf/quad) (remove nil?))
        annotations (rdf/annotation-subjects quads)
        quad-annotations (rdf/annotation-targets annotations quads)]
    (concat
     (when zn
       [(-> states
            first
            (select-keys [::en/env ::st/location ::rdf/graph])
            (assoc ::st/event ::st/subject-start ::rdf/stanza zn ::rdf/subject zn))])
     (->> states
          (map ::rdf/subject)
          distinct
          (remove #{zn})
          (concat [zn])
          (remove nil?)
          (sort-statements (dissoc grouped nil) annotations quad-annotations))
     (get grouped nil))))

(defn render-stanza
  "Given an environment and a sequence of states for a single stanza,
   return a sequence of states with rendered :output."
  [previous-states states]
  (->> states
       (remove #(contains? #{::st/subject-start ::st/subject-end} (::st/event %)))
       sort-stanza
       (reductions
        (fn [previous-state state]
          (->> state
               (st/update-state previous-state)
               render-state))
        (last previous-states))
       rest))

(defn render-states
  [previous-state states]
  (->> states
       (partition-by ::rdf/stanza)
       ;;(interpose [{::st/event ::st/blank}])
       (reductions
        (fn [previous-stanza stanza]
          (render-stanza previous-stanza stanza))
        [previous-state])
       rest
       (mapcat identity)))
