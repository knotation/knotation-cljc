(ns org.knotation.util
  (:require [clojure.string :as string]
            #?(:clj [clojure.data.json :as json])))

(defn starts-with?
  [target prefix]
  (and (>= (count target) (count prefix))
       (every? identity (map = target prefix))))

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

(defn throw-exception
  "Given a sequence of arguments,
    throw a cross-platform exception."
  [& messages]
  (throw
   (#?(:clj Exception. :cljs js/Error.)
    (->> messages
         (map str)
         (string/join " ")))))

(defmacro handler-case
  [body & handlers]
  `(try
     ~body
     ~@(map
        (fn [[exception-type name & body]]
          `(catch ~(if (= :default exception-type)
                     #?(:clj Exception :cljs js/Error)
                     exception-type) ~name ~@body))
        handlers)))
