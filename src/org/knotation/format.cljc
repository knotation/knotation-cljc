(ns org.knotation.format
  (:require [org.knotation.rdf :as rdf]
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

(defn read-mode-function
  [mode format lines]
  (let [format (or format :kn)
        func (get-in @formats [format ::read])]
    (if func
      (fn [states]
        (->> lines
             (func
              (-> states
                  last
                  (dissoc ::st/mode ::rdf/graph ::rdf/subject)
                  (merge (when mode {::st/mode mode}))))
             (concat states)))
      (fn [states]
        (concat
         states
         [{::st/error
           {::st/error-type :unknown-read-format
            ::st/error-message
            (str "Unknown read format: " format)}}])))))

(defn read-function
  [format lines]
  (read-mode-function nil format lines))

; TODO: improve on this implementation
(defn read-env-function
  [format lines]
  (read-mode-function :env format lines))

(defn read-prefixes-function
  [format lines]
  (comp
   (space-function)
   (fn [states]
     (for [{:keys [::st/event] :as state} states]
       (if (= event ::st/prefix)
         (dissoc state ::st/mode)
         state)))
   (read-mode-function :env format lines)))

; TODO: improve on this implementation
(defn read-data-function
  [format lines]
  (read-mode-function :data format lines))

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
