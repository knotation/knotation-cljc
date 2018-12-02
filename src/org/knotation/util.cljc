(ns org.knotation.util
  (:require [clojure.string :as string]
            #?(:clj [clojure.data.json :as json])))

(defn get-lines
  [s]
  (re-seq #".*\n?" s))

(defn split-lines
  "Split a string on newlines,
   keeping the newline with the string."
  [s]
  (butlast (get-lines s)))

(defn surround
  [before after xs]
  (concat [before] xs [after]))

(defn append
  [after xs]
  (concat xs [after]))

; See https://stackoverflow.com/a/42882784
(defn partition-with
  "Like split-with for multiple splits."
  [f coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (let [run (cons (first s) (take-while (complement f) (next s)))]
       (cons run (partition-with f (seq (drop (count run) s))))))))

(defn edn->json
  [content]
  #?(:clj (json/write-str content)
     :cljs (.stringify js/JSON (clj->js content) nil 2)))

(defn error
  [error-type & info]
  (->> info
       (map str)
       ;(concat [(get error-messages error-type "ERROR:")])
       (string/join " ")
       (assoc
        {:org.knotation.state/error-type error-type}
        :org.knotation.state/error-message)
       (merge (when info {:org.knotation.state/error-info info}))
       (assoc
        {:org.knotation.state/event :org.knotation.state/error}
        :org.knotation.state/error)))

(defn throw-exception
  "Given a sequence of arguments,
    throw a cross-platform exception."
  [& messages]
  (throw
   (#?(:clj Exception. :cljs js/Error.)
    (->> messages
         (map str)
         (string/join " ")))))
