(ns org.knotation.link)

(defn label->iri
  [env input]
  (when (string? input)
    (get-in env [:label-iri input])))

(defn wrapped-iri->iri
  [env input]
  (when (string? input)
    (when-let [[_ iri] (re-matches #"<(\S+)>" input)]
      iri)))

(defn http-url->iri
  [env input]
  (when (string? input)
    (when (re-matches #"https?://\S+" input)
      input)))

(defn curie->iri
  [env input]
  (when (string? input)
    (when-let [[_ prefix suffix] (re-matches #"(\S+):(\S+)" input)]
               ; TODO: (re-matches #"([a-zA-Z0-9]+):([^\s:/][^\s:\\]*)" input)]
      (when-let [iri (get-in env [:prefix-iri prefix])]
        (str iri suffix)))))

(defn subject->iri
  [env input]
  (or (wrapped-iri->iri env input)
      (label->iri env input)
      (curie->iri env input)
      (http-url->iri env input)))

(defn subject->node
  [env input]
  (or (when-let [iri (subject->iri env input)]
        {:iri iri})
      (when (re-matches #"_:\S+" input)
        {:bnode input})))

(defn predicate->iri
  [env input]
  (or (wrapped-iri->iri env input)
      (label->iri env input)
      (curie->iri env input)
      (http-url->iri env input)))

(defn object->node
  [env input]
  (or (when-let [iri (subject->iri env input)]
        {:iri iri})
      (when (re-matches #"_:\S+" input)
        {:bnode input})))

(defn iri->wrapped-iri
  [env iri]
  (str "<" iri ">"))
