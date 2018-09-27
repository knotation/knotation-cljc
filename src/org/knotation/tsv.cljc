(ns org.knotation.tsv
  (:require [clojure.string :as string]
            [org.knotation.state :as st]
            [org.knotation.format :as fm]
            [org.knotation.kn :as kn]))

(defn parse-cell
  [header cell]
  [::kn/statement-block
   [:name header]
   [:lexical cell]])

(defn parse-line
  [headers subject-index line]
  (let [cells (string/split line #"\t")]
    (concat
     [[::kn/subject-line
       [:name (get cells subject-index)]]]
     (for [i (remove #(= subject-index %) (range 0 (count cells)))]
       (parse-cell (get headers i) (get cells i))))))

(defn parse-lines
  [lines]
  (let [headers (string/split (first lines) #"\t")
        subject-index (->> headers
                           (map-indexed (fn [i x] [i x]))
                           (filter #(= "CURIE" (second %)))
                           first
                           first)]
    (mapcat (partial parse-line headers subject-index) (rest lines))))

; Implement format interface, mostly reusing Knotation format

(defmethod fm/parse-lines
  :tsv
  [fmt lines]
  (parse-lines lines))

(defmethod fm/process-parses
  :tsv
  [fmt parses]
  (kn/process-parses parses))

(defmethod fm/process-states
  :tsv
  [fmt states]
  (kn/process-states states))

(defmethod fm/read-parse
  :tsv
  [fmt env parse]
  (kn/read-parse env parse))

(defmethod fm/expand-state
  :tsv
  [fmt env state]
  (kn/expand-state env state))
