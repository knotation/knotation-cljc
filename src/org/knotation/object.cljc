(ns org.knotation.object
  (:require [org.knotation.link :as ln]))

(defn string->object
  [env datatype content]
  (cond
    (nil? datatype)
    {:lexical content}

    (and (string? datatype)
         (re-matches #"@\S+" datatype))
    {:lexical content :language (clojure.string/replace datatype #"^@" "")}

    (and (string? datatype)
         (re-matches #"https://knotation.org/format/\S+" datatype))
    (case datatype
      "https://knotation.org/format/link"
      (ln/object->node env content))

    :else
    {:lexical content :datatype datatype}))

(defn object->nquads-object
  [{:keys [lexical datatype language] :as node}]
  (cond
    language (str "\"" lexical "\"@" language)
    datatype (str "\"" lexical "\"^^<" datatype ">")
    :else (str "\"" lexical "\"")))
