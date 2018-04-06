(ns org.knotation.clj-api-test
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [org.knotation.clj-api :as api]))

(deftest test-nt->edn
  (->> "<s> <p> <o> .
<s> <p> \"o\" .
<s> <p> \"o\"@l .
<s> <p> \"o\"^^<d> ."
       (api/read-string :nt nil)
       (= [{:si "s" :pi "p" :oi "o"}
           {:si "s" :pi "p" :ol "o" :di "http://www.w3.org/2001/XMLSchema#string"}
           {:si "s" :pi "p" :ol "o" :ln "l"}
           {:si "s" :pi "p" :ol "o" :di "d"}])
       is))
