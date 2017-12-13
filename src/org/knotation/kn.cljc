(ns org.knotation.kn
  (:require [clojure.string :as string]
            [org.knotation.util :as util]
            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]
            [org.knotation.link :as ln]
            [org.knotation.object :as ob]
            [org.knotation.format :as fm]))

(defn read-comment
  [state]
  (if (-> state
          ::st/input
          ::st/lines
          first
          (util/starts-with? "#"))
    (assoc
     state ::st/event ::st/comment
     ::st/comment (-> state ::st/input ::st/lines first))
    (st/error state :not-a-comment)))

(defn read-declaration
  [{:keys [::st/mode ::en/env] :as state}]
  (if-let [[_ prefix iri]
           (->> state
                ::st/input
                ::st/lines
                first
                (re-matches #"@prefix (\S+):\s+<(\S+)>\s*"))]
    (let [state (assoc
                 state
                 ::st/event ::st/prefix
                 ::st/prefix prefix)]
      (if (= mode :data)
        state
        (st/add-prefix state prefix iri)))
    (st/error state :not-a-prefix-line)))

(defn read-subject
  [{:keys [::en/env] :as state}]
  (if-let [[_ subject]
           (->> state
                ::st/input
                ::st/lines
                first
                (re-matches #": \s*(.*)\s*"))]
    (assoc
     state
     ::rdf/subject (ln/subject->node env subject)
     ::st/event ::st/subject-start)
    (st/error state :not-a-subject-line)))

(def match-statement #"([^@:].*?)(; (.*))?: (.*)")
(def match-statement-1 #"([^@:].*?)(; (.*))?:(.*)")

(defn parse-statement
  [{:keys [::st/input] :as state}]
  (let [lines (::st/lines input)
        line (last lines)
        [_ predicate-name _ datatype-name initial-content]
        (re-matches match-statement-1 (first lines))
        initial-content
        (string/replace initial-content #"^ " "")
        content
        (->> lines
             rest
             (map #(string/replace % #"^ " ""))
             (concat [initial-content])
             (string/join "\n"))]
    (assoc
     state
     ::predicate-name predicate-name
     ::datatype-name datatype-name
     ::content content)))

(defn read-statement
  [{:keys [::st/mode ::en/env ::rdf/graph ::rdf/subject] :as state}]
  (if-let [[_ predicate-name _ datatype-name content]
           (->> state
                ::st/input
                ::st/lines
                first
                (re-matches match-statement))]
    (let [predicate-iri (ln/predicate->iri env predicate-name)
          language (when (util/starts-with? datatype-name "@")
                     (string/replace datatype-name #"^@" ""))
          datatype (when-not (util/starts-with? datatype-name "@")
                     datatype-name)
          datatype-iri (when datatype (ln/datatype->iri env datatype))
          content
          (->> state
               ::st/input
               ::st/lines
               rest
               (map #(string/replace % #"^ " ""))
               (concat [content])
               (string/join "\n"))]
      (cond
        (nil? predicate-iri)
        (st/error state :unrecognized-predicate predicate-name)

        (and datatype (nil? datatype-iri))
        (st/error state :unrecognized-datatype datatype-name)

        :else
        (let [predicate {::rdf/iri predicate-iri}
              object
              (ob/string->object
               env
               (or language
                   (get-in env [::en/predicate-language predicate-iri]))
               (or datatype-iri
                   (get-in env [::en/predicate-datatype predicate-iri])) content)
              quad {::rdf/graph graph
                    ::rdf/subject subject
                    ::rdf/predicate predicate
                    ::rdf/object object}
              state (assoc state ::st/event ::st/statement)
              state (if (= mode :data)
                      state
                      (st/update-state state quad))]
          (if (= mode :env) state (assoc state ::rdf/quads [quad])))))
    (st/error state :not-a-statement)))

(declare make-state)
(declare read-state)

(defn expand-template
  [{:keys [::en/env ::rdf/quads] :as state}]
  (if-let [{:keys [::rdf/predicate ::rdf/object] :as quad} (first quads)]
    (if (= (::rdf/iri predicate) "https://knotation.org/predicate/apply-template")
      (let [value (::rdf/lexical object)
            template (->> value
                          string/split-lines
                          first
                          string/trim
                          (ln/predicate->iri env))
            content (get-in env [::en/template-content template])
            values (->> value
                        string/split-lines
                        rest
                        (map #(string/split % #": "))
                        (into {}))
            result (string/replace
                    content
                    #"\{(.*?)\}"
                    (fn [[_ x]] (get values x)))
            states
            (->> result
                 string/split-lines
                 rest
                 (map-indexed (fn [i line] [(inc i) line]))
                 (reductions
                  (fn [previous line]
                    (read-state (make-state previous [line])))
                  state)
                 rest)
            quads
            (assoc-in
             quads
             [0 ::rdf/predicate ::rdf/iri]
             "https://knotation.org/predicate/applied-template")]
        (assoc (last states) ::rdf/quads (concat quads (mapcat ::rdf/quads states))))
      state)
    state))

(defn read-state
  [state]
  (case (->> state ::st/input ::st/lines first first)
    nil (assoc state ::st/event ::st/space)
    \# (read-comment state)
    \@ (read-declaration state)
    \: (read-subject state)
    (expand-template (read-statement state))))

(defn make-state
  [{:keys [::st/mode ::en/env ::rdf/graph ::rdf/subject]} lines]
  (merge
   {::en/env env}
   (when graph
     {::rdf/graph graph})
   (when subject
     {::rdf/subject subject})
   (when lines
     {::st/input
      {::st/format :kn
       ::st/line-number (ffirst lines)
       ::st/lines (map second lines)}})
   (when mode
     {::st/mode mode})))

(defn read-input-states
  [env input-states]
  (->> input-states
       (reductions
        (fn [previous current]
          (read-state
           (merge
            (select-keys previous [::en/env ::rdf/graph ::rdf/subject])
            current)))
        {::en/env env
         ::st/event ::st/graph-start})
       fm/insert-graph-events
       fm/insert-subject-events))

(defn read-input
  [env
   {:keys [::st/mode ::st/line-number ::st/lines]
    :or {line-number 1}
    :as input}]
  (let [input (-> input
                  (select-keys [::st/format ::st/source])
                  (assoc ::st/format :kn))]
    (->> lines
         (map-indexed (fn [i line] [(+ line-number i) line]))
         (util/partition-with #(not (util/starts-with? (second %) " ")))
         (map
          (fn [numbered-lines]
            (assoc
             input
             ::st/line-number (ffirst numbered-lines)
             ::st/lines (map second numbered-lines))))
         (map (fn [input] (merge {::st/input input} (when mode {::st/mode mode}))))
         (read-input-states env))))

(defn render-quad
  [env {:keys [::rdf/predicate ::rdf/object] :as quad}]
  (let [{:keys [::rdf/iri ::rdf/lexical ::rdf/language ::rdf/datatype]}
        object
        piri (::rdf/iri predicate)
        default-datatype (get-in env [::en/predicate-datatype piri])
        default-language (get-in env [::en/predicate-language piri])
        lines (when lexical (string/split lexical #"\n" -1))]
    (concat
     [(str
       (ln/node->name env predicate)
       (cond
         (and datatype (not= datatype default-datatype))
         (str "; " (ln/iri->name env datatype))

         (and language (not= language default-language))
         (str "; @" language))
       ": "
       (if iri (ln/iri->name env iri) (first lines)))]
     (->> lines
          rest
          (map (partial str " "))))))

(defn output-lines
  [state lines]
  (assoc
   state
   ::st/output
   {::st/format :kn
    ::st/lines lines}))

(defn render-state
  [{:keys [::st/mode ::st/event
           ::en/env ::en/env-before
           ::st/comment
           ::st/prefix
           ::rdf/quads
           ::rdf/subject ::st/previous-subject]
    :as state}]
  (case (if (= :env mode) nil event)
    ::st/comment
    (output-lines state [comment])

    ::st/prefix
    (->> (get-in env [::en/prefix-iri prefix])
         (ln/iri->wrapped-iri nil)
         (str "@prefix " prefix ": ")
         vector
         (output-lines state))

    ::st/space
    (output-lines state [""])

    ::st/subject-start
    (output-lines state [(str ": " (ln/node->name env subject))])

    ::st/statement
    (output-lines state (mapcat (partial render-quad env) quads))

    state))

(defn render-states
  [states]
  (->> states
       (map render-state)
       fm/number-output-lines))

(fm/register!
 {::fm/name :kn
  ::fm/description "Knotation format"
  ::fm/read read-input
  ::fm/render render-states})
