(ns org.knotation.util
  (:require [clojure.string :as string]))

(defn starts-with?
  [target prefix]
  (and (>= (count target) (count prefix))
       (every? identity (map = target prefix))))

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
