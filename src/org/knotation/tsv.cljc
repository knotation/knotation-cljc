(ns org.knotation.tsv
  (:require [clojure.string :as string]
            [org.knotation.util :as util]
            [org.knotation.state :as st]
            [org.knotation.kn :as kn]))

(defn split-row
  "Split a row string on tabs."
  [s]
  (string/split s #"\t" -1))

(defn split-cell
  "Split a cell string on pipes."
  [s]
  (string/split s #"\|" -1))

(defn format-header-value
  "Given a header value, append a colon if necessary,
   and return the header value."
  [s]
  (if (re-find #"(:|;)\s*$" s) s (str s ":")))

(defn collect-header-values
  "Given a header cell, return a vector of header values or nil (if blank)."
  [s]
  (when-not (string/blank? s)
    (->> s
         split-cell
         (mapv string/trimr)
         (mapv format-header-value))))

(defn collect-headers
  "Given a header line, return a nested vector of header values."
  [line]
  (->> line
       split-row
       (mapv collect-header-values)))

(defn parse-cell
  "Given a map with location information, headers, and a cell string,
   return a sequence of states for this cell."
  [{:keys [::row ::column ::headers ::cell] :as coll}]
  (->> (for [header headers
             value (split-cell cell)]
         [header value])
       (reductions
        (fn [{:keys [::st/location] :as coll} [h v]]
          (when (and (not (string/blank? h))
                     (not (string/blank? v)))
            (-> st/default-state
                (assoc ::st/location location)
                (st/input :tsv v)
                (assoc-in [::st/input ::row] row)
                (assoc-in [::st/input ::column] column)
                (assoc ::st/parse (kn/parse-line (str h " " v)))
                (assoc ::st/event ::st/parse))))
        (assoc coll ::st/location (::st/start coll)))
       rest))

(defn parse-row
  "Given a row number, a sequence of headers, and a row string,
   return a sequence of states for this row."
  [row headers line]
  (->> line
       split-row
       (zipmap headers)
       (reductions
        (fn [{:keys [::st/location] :as coll} [hs cell]]
          (-> coll
              (update ::column inc)
              (assoc ::headers hs)
              (assoc ::cell cell)
              (assoc ::st/start location)
              (update ::st/location st/advance-location cell)
              (update ::st/location st/step-location)))
        {::row row
         ::column 0
         ::st/location {::st/line-number row ::st/column-number 0}})
       rest
       (mapcat parse-cell)
       (remove nil?)))

(defn parse-lines
  "Given a sequence of lines, return a sequence of states."
  [lines]
  (let [headers (collect-headers (first lines))]
    (->> lines
         rest
         (map-indexed (fn [i line] (parse-row (+ i 2) headers line)))
         (mapcat identity))))

(defn parse-input
  "Given an input string, return a sequence of states."
  [s]
  (parse-lines (string/split s #"\n" -1)))

(defn read-lines
  "Given a initial state and a sequence of lines (strings)
   return a sequence of states."
  [initial-state lines]
  (->> lines
       parse-lines
       kn/merge-indented
       (kn/read-parses initial-state)
       st/insert-subject-events
       st/insert-stanza-events
       st/insert-stanza-separators))

(defn read-input
  "Given an initial state and a string,
   return a sequence of states."
  [initial-state input]
  (->> input
       util/split-lines
       (read-lines initial-state)))
