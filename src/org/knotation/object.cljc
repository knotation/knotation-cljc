(ns org.knotation.object
  (:require [clojure.string :as string]
            [org.knotation.util :as util]
            [org.knotation.link :as ln]))

(defn string->object
  [env datatype content]
  (cond
    (nil? datatype)
    {:lexical content}

    (and (string? datatype)
         (re-matches #"@\S+" datatype))
    {:lexical content :language (string/replace datatype #"^@" "")}

    (and (string? datatype)
         (util/starts-with? datatype "https://knotation.org/datatype/"))
    (case datatype
      "https://knotation.org/datatype/link"
      (ln/object->node env content))

    :else
    {:lexical content :datatype datatype}))

(defn nquads-literal->object
  [content]
  (or
   (when-let [[_ lexical iri] (re-matches #"\"(.*)\"\^\^<(\S+)>\s*" content)]
     {:lexical lexical :datatype iri})
   (when-let [[_ lexical lang] (re-matches #"\"(.*)\"@(\S+)\s*" content)]
     {:lexical lexical :language lang})
   (when-let [[_ lexical] (re-matches #"\"(.*)\"\s*" content)]
     {:lexical lexical})
   (throw (Exception. (str "Bad NQuads object: " content)))))

(defn nquads-object->object
  [content]
  (case (first content)
    \< {:iri (ln/wrapped-iri->iri nil content)}
    \_ {:bnode content}
    \" (nquads-literal->object content)
    (throw (Exception. (str "Bad NQuads object: " content)))))

(defn object->nquads-object
  [{:keys [lexical datatype language] :as node}]
  (cond
    language (str "\"" lexical "\"@" language)
    datatype (str "\"" lexical "\"^^<" datatype ">")
    :else (str "\"" lexical "\"")))
