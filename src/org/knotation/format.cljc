(ns org.knotation.format
  (:require [clojure.string :as string]
            [org.knotation.util :as util]
            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]))

; The basic steps for reading are:
; - parse a line (string) to a parse vector
; - use an environment to read the parse vector into a state
; - use the state to update the environment for the next read
;
; The basic steps for writing are:
; - use an envionment to render a state to a parse vector
; - use the state to update the environment for the next render
; - render the parses (flatten, filter for strings, joining them)
;
; Each format must provide:
; - (parse-line line) :: String -> {}
; - (read-parse env parse) :: Env -> {:env :input} -> {:env :input ...}
; - (render-state env state) :: Env -> {:env :input ...} -> ParseVector
;
; Formats may optionally provide:
; - (process-parses parses) :: [{:env :input ...}] -> [{:env :input ...}]
; - (expand-state env state) :: Env -> {:env :input ...} -> ({:env :input ...} ParseVector)
; - (render-states env states) :: Env -> [{:env :input ...}] -> [{:env :input (:output :: {:parse...}) ...}]

; Parse handling functions

(defn flatten-parses
  "Given a parse vector (or sequence of parse vectors),
   return the content as a lazy sequence of strings."
  [parses]
  (->> parses
       flatten
       (filter string?)))

(defn render-parses
  "Given a parse vector (or sequence of parse vectors),
   return the content as a single string."
  [parses]
  (->> parses
       flatten-parses
       string/join))

(defn render-lines
  "Given a sequence of states with :output :parse values,
   return a lazy sequence of lines."
  [states]
  (->> states
       (map :output)
       (map :parse)
       (map flatten-parses)
       (map string/join)))

(defn render-output
  "Given a sequence of states with :output :parse values,
   return a sequence of string contents."
  [states]
  (->> states
       (map :output)
       (map :parse)
       render-parses))

(defn locate-parse
  "Given an initial line number and column number, and a parse vector,
   add lines and columns to each parse element,
   and return the updated parse vector."
  [line column parse]
  (->> parse
       rest
       rest
       (reductions
        (fn [[x y line column] [k v]]
          (if (= :eol x)
            [k v (inc line) 1]
            [k v line (+ column (count y))]))
        [(-> parse second first) (-> parse second second) line column])
       (apply conj [(first parse)])))

(defn locate-parses
  "Given an initial line number and column number, and a sequence of parse vectors,
   add lines and columns to each parse element,
   and return a lazy sequence of updated parse vectors."
  [line column parses]
  (->> parses
       rest
       (reductions
        (fn [previous parse]
          (let [last-element (last previous)]
            (if (= :eol (first last-element))
              (locate-parse (inc (nth last-element 2)) 1 parse)
              (locate-parse (nth last-element 2) (nth last-element 3) parse))))
        (locate-parse line column (first parses)))))

; Interface: Step by Step
;
; Each format should implement these multimethods.

(defmulti parse-line
  "Given a format keyword and a line, return a parse vector."
  (fn [fmt line] fmt))

(defmulti process-parses
  "Given a format keyword and a sequence of parse vectors,
   do any format-specific parse processing,
   and return a lazy sequence of parse vectors."
  (fn [fmt parses] fmt))

(defmethod process-parses
  :default
  [fmt parses]
  parses)

(defmulti read-parse
  "Given a format keyword, and environment (or nil), and a parse vector,
   return a state."
  (fn [fmt env parse] fmt))

(defmulti expand-state
  "Given a format keyword, an environment (or nil), and a state,
   return the pair of an updated state and a sequence of new parse vectors (maybe empty)."
  (fn [fmt env state] fmt))

(defmethod expand-state
  :default
  [fmt env state]
  [state])

(defmulti render-state
  "Given a format keyword, and environment (or nil), and a state,
   return a parse vector."
  (fn [fmt env state] fmt))

; Insert events

(def subject-keys [::en/env :gi :si :sb])

(defn insert-subject-start
  "Given a sequence of states with the same subject,
   ensure that the subject-start state is present."
  [states]
  (let [{first-event ::st/event :as first-state} (first states)
        subject (:si first-state)]
    (concat
     (when (and subject (not= first-event ::st/subject-start))
       [(-> first-state
            (select-keys subject-keys)
            (assoc ::st/event ::st/subject-start))])
     states)))

(defn insert-subject-end
  "Given a sequence of states with the same subject,
   ensure that the subject-end state is present.
   If the last state is a blank line, it will still be last
   but without a subject."
  [states]
  (let [{last-event ::st/event :as last-state} (last states)
        subject (:si last-state)]
    (concat
     (if (= last-event ::st/blank)
       (butlast states)
       states)
     (when (and subject (not= last-event ::st/subject-end))
       [(-> last-state
            (select-keys subject-keys)
            (assoc ::st/event ::st/subject-end))])
     (when (= last-event ::st/blank)
       [(dissoc last-state ::rdf/si)]))))

(defn insert-subject-events
  "Given a sequence of states,
   ensure that subject-start and subject-end states are present."
  [states]
  (->> states
       (partition-by :si)
       (map insert-subject-start)
       (mapcat insert-subject-end)))

(def graph-keys [::en/env :gi])

(defn insert-graph-start
  "Given a sequence of states with the same graph,
   ensure that the graph-start state is present."
  [states]
  (let [{first-event ::st/event :as first-state} (first states)
        graph (:gi first-state)]
    (concat
     (when (not= first-event ::st/graph-start)
       [(-> first-state
            (select-keys graph-keys)
            (assoc ::st/event ::st/graph-start))])
     states)))

(defn insert-graph-end
  "Given a sequence of states with the same graph,
   ensure that the graph-end state is present."
  [states]
  (let [{last-event ::st/event :as last-state} (last states)
        graph (:gi last-state)]
    (concat
     states
     (when (not= last-event ::st/graph-end)
       [(-> last-state
            (select-keys graph-keys)
            (assoc ::st/event ::st/graph-end))]))))

(defn insert-graph-events
  "Given a sequence of states,
   ensure that graph-start and graph-end states are present."
  [states]
  (->> states
       (partition-by :gi)
       (map insert-graph-start)
       (mapcat insert-graph-end)))

; Interface: All at Once
;
; These functions are shared across all formats.

(defmulti parse-lines
  "Given a format keyowrd and a sequence of lines,
   return a lazy sequence of parse vectors."
  (fn [fmt lines] fmt))

(defmethod parse-lines
  :default
  [fmt lines]
  (->> lines
       (map (partial parse-line fmt))
       (process-parses fmt)))

(defmulti process-states
  "Given a format keyword and a sequence of states, do any format-specific
   post-processing on finished states, and return a lazy sequence of
   potentially modified states."
  (fn [fmt states] fmt))

(defmethod process-states
  :default
  [fmt states]
  states)

(defn inner-render-states
  "Given an initial environment, a format keyword, and a sequence of states,
   return a lazy sequence of [env state parse] triples."
  [fmt env states]
  (->> states
       (reductions
        (fn [[previous-env previous-state _] state]
          (let [previous-env (or previous-env en/blank-env)
                previous-state (or previous-state st/blank-state)
                env (st/update-env previous-env previous-state)]
            [env state (render-state fmt env state)]))
        [env nil nil])
       rest))

(defmulti render-states
  "Given an initial environment, a format keyword, and a sequence of states,
   return a lazy sequence of states with :output :parse values."
  (fn [fmt env states] fmt))

(defmethod render-states
  :default
  [fmt env states]
  (->> states
       (inner-render-states fmt env)
       ;(map last)))
       (map (fn [[env state parse]] (assoc state ::en/env env :output {:parse parse})))))
