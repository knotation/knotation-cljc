(ns org.knotation.tsv
  (:require [clojure.string :as string]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]
            [org.knotation.format :as fm]
            [org.knotation.kn :as kn]))

(def input-keys [::st/source])

(defn group-lines
  "Given an input map, return a sequence of input-states."
  [{:keys [::st/mode ::st/line-number ::st/lines]
    :or {line-number 1}
    :as input}]
  (let [input (-> input
                  (select-keys input-keys)
                  (assoc ::st/format :tsv))
        headers (string/split (first lines) #"\t" -1)]
    (->> lines
         rest
         (map-indexed
          (fn [i line]
            (map-indexed
             (fn [j cell]
               (assoc
                input
                ::st/line-number (+ line-number i 1)
                ::st/column-number (inc j)
                ::st/lines [(str (get headers j) ": " cell)]))
             (string/split line #"\t" -1))))
         (mapcat identity)
         (map (fn [x] (merge {::st/input x} (when mode {::st/mode mode})))))))

(defn read-input
  [env {:keys [::st/line-number ::st/lines] :or {line-number 1} :as input}]
  (let [header-line (first lines)]
    (->> input
         group-lines
         kn/merge-multilines
         (kn/read-input-states env)
         (concat
          [{::st/event ::st/header
            ::en/env env
            ::st/input
            (assoc
             (select-keys input input-keys)
             ::st/format :tsv
             ::st/line-number line-number
             ::st/lines [header-line])}])
         fm/insert-graph-events
         fm/insert-subject-events)))

(fm/register!
 {::fm/name :tsv
  ::fm/description "Knotation TSV format"
  ::fm/read read-input})
