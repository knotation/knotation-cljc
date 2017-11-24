(ns org.knotation.kn
  (:require [clojure.string :as string]
            [org.knotation.util :as util]
            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]
            [org.knotation.link :as ln]
            [org.knotation.object :as ob]
            [org.knotation.format :as fm]))

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

(defn read-state
  [state]
  (case (->> state ::st/input ::st/lines first first)
    nil (assoc state ::st/event ::st/space)
    \# (assoc state ::st/event ::st/comment)
    \@ (read-declaration state)
    \: (read-subject state)
    (read-statement state)))

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

(defn read-subject-lines
  [states numbered-lines]
  (->> numbered-lines
       (util/partition-with #(not (util/starts-with? (second %) " ")))
       (util/append ::st/subject-end)
       (reductions
        (fn [previous item]
          (if (keyword? item)
            (assoc (make-state previous nil) ::st/event item)
            (read-state (make-state previous item))))
        (last states))
       rest))

(defn read-grouped-lines
  [states numbered-lines]
  (if (util/starts-with? (second (first numbered-lines)) ": ")
    (read-subject-lines states numbered-lines)
    (->> numbered-lines
         (reductions
          (fn [previous item]
            (read-state (make-state previous [item])))
          (last states))
         rest)))

(defn read-graph
  [states numbered-lines]
  (->> numbered-lines
       (util/partition-with #(util/starts-with? (second %) ": "))
       (util/surround ::st/graph-start ::st/graph-end)
       (reductions
        (fn [subject-states lines]
          (if (keyword? lines)
            (-> subject-states
                last
                (make-state nil)
                (dissoc ::rdf/subject)
                (assoc ::st/event lines)
                vector)
            (read-grouped-lines subject-states lines)))
        [(assoc (last states) ::rdf/graph nil)]) ; TODO: add graph support
       rest
       (mapcat identity)))

(defn read-lines
  [state lines]
  (->> lines
       (map-indexed (fn [i line] [(inc i) line]))
       (util/partition-with #(util/starts-with? (second %) "@graph"))
       (reductions read-graph [state])
       rest
       (mapcat identity)))

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
           ::st/prefix
           ::rdf/quads
           ::rdf/subject ::st/previous-subject]
    :as state}]
  (case (if (= :env mode) nil event)
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
  ::fm/read read-lines
  ::fm/render render-states})
