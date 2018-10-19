(ns org.knotation.kn
  (:require [clojure.string :as string]
            [org.knotation.util :as util]
            [instaparse.core :as insta]
            [org.knotation.util :as util]
            [org.knotation.rdf :as rdf :refer [owl rdf kn]]
            [org.knotation.environment :as en]
            [org.knotation.link :as ln]
            [org.knotation.format :as fm]
            [org.knotation.omn :as omn]))

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
    (if-let [[predicate datatype] (string/split pd #"; " 2)]
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
      "https://knotation.org/kn/link"
      (ln/object->node env content)

      "https://knotation.org/kn/omn"
      (let [res (omn/read-class-string env content)]
        (merge
         {:states (map #(assoc % :di datatype) res)
          :di datatype}
         (omn/->obj env res)))

      ; TODO: warn on unrecognized Knotation datatype
      ;(string/starts-with? datatype "https://knotation.org/kn/")

      {:ol content :di datatype})

    :else {:ol content}))

(defn read-object
  "Read the object part of a statement, with its language or datatype,
   and using the default language for its predicate."
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
  (cond
    (and ln (not= ln (en/get-language env predicate-iri)))
    [[:symbol ";"]
     [:space " "]
     [:name (str "@" ln)]]
    (and di (not= di (en/get-datatype env predicate-iri)))
    [[:symbol ";"]
     [:space " "]
     [:name (ln/iri->name env di)]]
    :else
    []))

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

(def -blank-node-table (atom {}))
(defn blank-node-of [target]
  (let [target (select-keys target [:rt :gi :si :sb :pi :oi :ob :ol :di :ln])]
    (get @-blank-node-table target)))
(defn blank-node-of! [target]
  (let [target (select-keys target [:rt :gi :si :sb :pi :oi :ob :ol :di :ln])]
    (if (not (get @-blank-node-table target))
      (swap! -blank-node-table #(assoc % target (rdf/random-blank-node))))
    (get @-blank-node-table target)))

(defn process-annotations
  [states]
  (mapcat
   (fn [state]
     (if (= :annotation (:event state))
       (let [{:keys [:si :sb] :as target} (:target state)
             b1 (blank-node-of! state)
             source (if-let [bnode (blank-node-of target)]
                      {:ob bnode}
                      (if si {:oi si} {:ob sb}))]
         [(dissoc state :stack :level)
          {:sb b1 :pi (rdf "type") :oi (owl "Annotation")}
          (merge {:sb b1 :pi (owl "annotatedSource")} source)
          {:sb b1 :pi (owl "annotatedProperty") :oi (:pi target)}
          (merge {:sb b1 :pi (owl "annotatedTarget")} (select-keys target [:oi :ob :ol]))
          (merge {:sb b1} (select-keys state [:pi :oi :ob :ol]))])
       [state]))
   ((fn rec [ss]
      (when (not (empty? (rest ss)))
        (let [res (process-annotation (first ss) (second ss))]
          (lazy-seq (cons res (rec (cons res (rest (rest ss)))))))))
    (cons nil states))))

(defn process-class-expressions
  [states]
  (mapcat
   (fn [state]
     (if (and (= :statement (:event state))
              (= "https://knotation.org/kn/omn" (:di state)))
       (cons (dissoc state :states)
             (:states state))
       [state]))
   states))

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
       (map merge-parses)
       (map vec)))

(defn process-stanza-labels
  [states]
  (let [top-subject (atom nil)]
    (map
     (fn [s]
       (case (:event s)
         :subject-start (assoc s :zi (reset! top-subject (or (:si s) (:sb s))))
         :subject-end (let [res (assoc s :zi @top-subject)]
                        (reset! top-subject nil)
                        res)
         (if-let [zi @top-subject] (assoc s :zi zi) s)))
     states)))

(defn process-default-datatypes
  [states]
  (let [new-states
        ((fn rec [ss default-datatypes]
           (when (not (empty? ss))
             (let [s (first ss)
                   new-dds (if (and (:si s) (:oi s) (= (:pi s) (kn "default-datatype")))
                             (assoc default-datatypes (:si s) (:oi s))
                             default-datatypes)
                   new-env (assoc
                            (::en/env s) ::en/predicate-datatype
                            (merge
                             (::en/predicate-datatype (::en/env s))
                             default-datatypes))]
               (lazy-seq (cons (assoc s ::en/env new-env) (rec (rest ss) new-dds))))))
         states {})]
    new-states))

(defn state-line-count
  [s]
  (->> s :input :parse
       (filter #(and (vector? %) (= :eol (first %))))
       count))

(defn number-input-lines
  [states]
  (reductions
   (fn [prev cur]
     (let [ln (get-in prev [:input :line-number] 0)
           ct (get-in prev [:input :line-count] 0)
           cct (state-line-count cur)]
       (assoc cur :input
              (assoc (:input cur)
                     :line-count (if (zero? cct) ct cct)
                     :line-number (if (zero? cct) ln (+ ln ct))))))
   states))

(defn process-states
  [states]
  (->> states
       process-annotations
       process-class-expressions
       process-stanza-labels
       process-default-datatypes
       (remove nil?)
       number-input-lines))

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
  (if (= pi "https://knotation.org/kn/apply-template")
    (try
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
        [(assoc state :pi "https://knotation.org/kn/applied-template")
         (->> (string/replace
               (en/get-template-content env template-iri)
               #"\{(.*?)\}"
               (fn [[_ x]] (get values x)))
              util/split-lines
              rest
              (map parse-line)
              process-parses)])
      (catch Exception e (util/throw-exception "Template error for " ol " " e)))
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
  (process-states states))

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
