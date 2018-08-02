(ns org.knotation.nq
  (:require [clojure.string :as string]

            [org.knotation.environment :as en]
            [org.knotation.state :as st]
            [org.knotation.format :as fm]))

(defn render-object
  [env {:keys [:oi :ob :ol :di :ln] :as state}]
  ;; FIXME - add datatype and language annotations
  (let [o (or ob (and ol (str "\"" ol "\"")) (and oi (str "<" oi ">")))]
    (cond
      ln (str o "@" ln)
      di (str o "^^<" di ">")
      :else o)))

(defn number-output-lines
  [states]
  (reductions
   (fn [prev cur]
     (let [ln (get-in prev [:output :line-number] 1)]
       (assoc-in cur [:output :line-number]
                 (if (get-in prev [:output :parse])
                   (inc ln)
                   ln))))
   states))

(defmethod fm/process-states
  :nq [fmt states]
  (println "PROCESSING NQ STATES!")
  (->> states
       number-output-lines))

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

(defn inner-render-states
  "Given an initial environment, a format keyword, and a sequence of states,
   return a lazy sequence of [env state parse] triples."
  [fmt env states]
  (->> states
       (reductions
        (fn [[previous-env previous-state _] state]
          (let [env (st/update-env previous-env previous-state)]
            [env state (fm/render-state fmt env state)]))
        [env nil nil])
       rest))

(defmethod fm/render-states
  :nq
  [fmt env states]
  (->> states
       (inner-render-states fmt env)
       (map (fn [[env state parse]] (assoc state ::en/env env :output {:parse parse})))
       number-output-lines))
