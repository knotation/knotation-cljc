(ns org.knotation.nq
  (:require [clojure.string :as string]
            
            [org.knotation.format :as fm]))

(defn render-object
  [env {:keys [:oi :ob :ol :di :ln] :as state}]
  ;; FIXME - add datatype and language annotations
  (or ob (and ol (str "\"" ol "\"")) (and oi (str "<" oi ">"))))

(defmethod fm/render-state
  :nq [fmt env {:keys [:gi :si :sb :pi :oi :ob :ol :di :ln] :as state}]
  (when (and pi (or sb si) (or oi ob ol))
    (->> [(or sb (and si (str "<" si ">")))
          (str "<" pi ">")
          (render-object env state)
          (and gi (str "<" gi ">"))]
         (remove nil?)
         (interpose " ")
         vec
         (#(conj % " .\n")))))
