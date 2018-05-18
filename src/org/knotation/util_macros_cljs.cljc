(ns org.knotation.util-macros-cljs)

(defmacro handler-case
  [body & handlers]
  `(try
     ~body
     ~@(map
        (fn [[exception-type name & body]]
          `(catch ~(if (= :default exception-type)
                     #?(:cljs js/Error :clj nil)
                     exception-type) ~name ~@body))
        handlers)))
