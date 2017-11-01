(ns org.knotation.util)

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
         (clojure.string/join " ")))))
