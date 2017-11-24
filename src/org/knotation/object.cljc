(ns org.knotation.object
  (:require [clojure.string :as string]
            [org.knotation.util :as util]
            [org.knotation.rdf :as rdf]
            [org.knotation.link :as ln]
            [org.knotation.omn :as omn]))

(defn string->object
  [env language datatype content]
  (cond
    (string? language)
    {::rdf/lexical content ::rdf/language language}

    (string? datatype)
    (case datatype
      "https://knotation.org/datatype/link"
      (ln/object->node env content)

      "https://knotation.org/datatype/omn"
      (omn/read-class-expression env content)

      ; TODO: warn on unrecognized Knotation datatype
      ;(util/starts-with? datatype "https://knotation.org/datatype/")

      {::rdf/lexical content ::rdf/datatype datatype})

    :else
    {::rdf/lexical content}))

(defn nquads-literal->object
  [content]
  (or
   (when-let [[_ lexical iri] (re-matches #"\"(.*)\"\^\^<(\S+)>\s*" content)]
     {::rdf/lexical (string/replace lexical "\\n" "\n")
      ::rdf/datatype iri})
   (when-let [[_ lexical lang] (re-matches #"\"(.*)\"@(\S+)\s*" content)]
     {::rdf/lexical (string/replace lexical "\\n" "\n")
      ::rdf/language lang})
   (when-let [[_ lexical] (re-matches #"\"(.*)\"\s*" content)]
     {::rdf/lexical (string/replace lexical "\\n" "\n")})
   (throw (Exception. (str "Bad NQuads object: " content)))))

(defn nquads-object->object
  [content]
  (case (first content)
    \< {::rdf/iri (ln/wrapped-iri->iri nil content)}
    \_ {::rdf/bnode content}
    \" (nquads-literal->object content)
    (throw (Exception. (str "Bad NQuads object: " content)))))

(defn object->nquads-object
  [{:keys [::rdf/lexical ::rdf/datatype ::rdf/language] :as node}]
  (let [lexical (if (re-find #"\n" lexical)
                  (string/replace lexical "\n" "\\n")
                  lexical)]
    (cond
      language (str "\"" lexical "\"@" language)
      datatype (str "\"" lexical "\"^^<" datatype ">")
      :else (str "\"" lexical "\""))))

(defn object->turtle-object
  [env {:keys [::rdf/lexical ::rdf/datatype ::rdf/language] :as node}]
  (let [lexical (if (re-find #"\n" lexical)
                  (str "\"\"\"" lexical "\"\"\"")
                  (str "\"" lexical "\""))]
    (cond
      language (str lexical "@" language)
      datatype (str lexical "^^" (ln/iri->curie-or-wrapped-iri env datatype))
      :else lexical)))
