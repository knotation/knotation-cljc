(ns org.knotation.format
  (:require [org.knotation.util :as util]
            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]))

(defonce formats (atom {}))

(def example-formats
  {"https://knotation.org/format/knotation"
   {::name "https://knotation.org/format/knotation"
    ::description "Foo"
    ::read identity
    ::render identity}})

(defn register!
  [{:keys [::name] :as format}]
  (swap! formats assoc name format))

(defn space-function
  []
  (fn [states]
    (concat
     states
     [{::st/event ::st/space
       ::en/env (-> states last ::en/env)}])))

(defn read-function
  [{:keys [::st/mode ::st/format ::st/lines] :as input}]
  (let [format (or format :kn)
        func (get-in @formats [format ::read])
        func (if (= mode :prefixes)
               (comp
                (space-function)
                (fn [states]
                  (for [{:keys [::st/event] :as state} states]
                    (if (= event ::st/prefix)
                      (dissoc state ::st/mode)
                      state)))
                func)
               func)
        mode (if (= mode :prefixes) :env mode)
        input (if mode (assoc input ::st/mode mode) input)]
    (if func
      (fn [states]
        (concat
         states
         (func (-> states last ::en/env) input)))
      (fn [states]
        (concat
         states
         [{::st/error
           {::st/error-type :unknown-read-format
            ::st/error-message
            (str "Unknown read format: " format)}}])))))

(defn render-function
  [format]
  (get-in
   @formats
   [(or format :nq) ::render]
   (fn [states]
     (map
      #(assoc
        %
        ::st/error
        {::st/error-type :unknown-render-format
         ::st/error-message
         (str "Unknown render format: " format)})
      states))))

(def subject-keys [::en/env ::st/mode ::rdf/graph ::rdf/subject])

(defn insert-subject-start
  "Given a sequence of states with the same subject,
   ensure that the subject-start state is present."
  [states]
  (let [{first-event ::st/event :as first-state} (first states)
        subject (::rdf/subject first-state)]
    (concat
     (when (and subject (not= first-event ::st/subject-start))
       [(-> first-state
            (select-keys subject-keys)
            (assoc ::st/event ::st/subject-start))])
     states)))

(defn insert-subject-end
  "Given a sequence of states with the same subject,
   ensure that the subject-end state is present."
  [states]
  (let [{last-event ::st/event :as last-state} (last states)
        subject (::rdf/subject last-state)]
    (concat
     states
     (when (and subject (not= last-event ::st/subject-end))
       [(-> last-state
            (select-keys subject-keys)
            (assoc ::st/event ::st/subject-end))]))))

(defn insert-subject-events
  "Given a sequence of states,
   ensure that subject-start and subject-end states are present."
  [states]
  (->> states
       (partition-by ::rdf/subject)
       (map insert-subject-start)
       (mapcat insert-subject-end)))

(def graph-keys [::en/env ::st/mode ::rdf/graph])

(defn insert-graph-start
  "Given a sequence of states with the same graph,
   ensure that the graph-start state is present."
  [states]
  (let [{first-event ::st/event :as first-state} (first states)
        graph (::rdf/graph first-state)]
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
        graph (::rdf/graph last-state)]
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
       (partition-by ::rdf/graph)
       (map insert-graph-start)
       (mapcat insert-graph-end)))

(defn sequential-blank-nodes
  [states]
  (->> states
       (reductions
        (fn [{:keys [::rdf/coll] :as previous} {:keys [::rdf/quads] :as current}]
          (if quads
            (let [results (reductions rdf/replace-blank-nodes coll quads)
                  coll (last results)
                  quads (map ::rdf/quad (rest results))]
              (assoc current ::rdf/coll coll ::rdf/quads quads))
            (assoc current ::rdf/coll coll)))
        {::rdf/coll {::rdf/counter 1}})
       rest
       (map #(dissoc % ::rdf/coll))))

(defn number-output-lines
  [states]
  (->> states
       (reductions
        (fn [{:keys [::line-number] :as previous} current]
          (let [lines (get-in current [::st/output ::st/lines] [])
                new-line-number (+ line-number (count lines))
                current (assoc current ::line-number new-line-number)]
            (if (::st/output current)
              (assoc-in current [::st/output ::st/line-number] line-number)
              current)))
        {::line-number 1})
       rest
       (map #(dissoc % ::line-number))))
