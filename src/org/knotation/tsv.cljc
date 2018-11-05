(ns org.knotation.tsv
  (:require [clojure.string :as string]
            [org.knotation.state :as st]
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

; TODO: Implement format interface, mostly reusing Knotation format
