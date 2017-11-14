(ns org.knotation.object
  (:require [clojure.string :as string]
            [org.knotation.util :as util]
            [org.knotation.rdf :as rdf]
            [org.knotation.link :as ln]))

(defn string->object
  [env lt-or-dt content]
  (cond
    (nil? lt-or-dt)
    {::rdf/lexical content}

    (and (string? lt-or-dt)
         (re-matches #"@\S+" lt-or-dt))
    {::rdf/lexical content ::rdf/language (string/replace lt-or-dt #"^@" "")}

    (and (string? lt-or-dt)
         (util/starts-with? lt-or-dt "https://knotation.org/datatype/"))
    (case lt-or-dt
      "https://knotation.org/datatype/link"
      (ln/object->node env content))

    :else
    {::rdf/lexical content ::rdf/datatype lt-or-dt}))

(defn nquads-literal->object
  [content]
  (or
   (when-let [[_ lexical iri] (re-matches #"\"(.*)\"\^\^<(\S+)>\s*" content)]
     {::rdf/lexical lexical ::rdf/datatype iri})
   (when-let [[_ lexical lang] (re-matches #"\"(.*)\"@(\S+)\s*" content)]
     {::rdf/lexical lexical ::rdf/language lang})
   (when-let [[_ lexical] (re-matches #"\"(.*)\"\s*" content)]
     {::rdf/lexical lexical})
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
  (cond
    language (str "\"" lexical "\"@" language)
    datatype (str "\"" lexical "\"^^<" datatype ">")
    :else (str "\"" lexical "\"")))
