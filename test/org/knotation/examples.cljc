(ns org.knotation.examples)

; # Basic Datatypes

(def basic-datatypes-kn
  "@prefix kn: <https://knotation.org/kn/>
@prefix ex: <http://example.com/>

: ex:s
ex:p; kn:link: ex:o
ex:p; kn:link: _:b0
ex:p: o
ex:p; ex:d: o
ex:p; @en: o
")

(def basic-datatypes-ttl
  "@prefix kn: <https://knotation.org/kn/> .
@prefix ex: <http://example.com/> .

ex:s
  ex:p ex:o ;
  ex:p _:b0 ;
  ex:p \"o\" ;
  ex:p \"o\"^^ex:d ;
  ex:p \"o\"@en .
")

(def basic-datatypes-nt
  "<http://example.com/s> <http://example.com/p> <http://example.com/o> .
<http://example.com/s> <http://example.com/p> _:b0 .
<http://example.com/s> <http://example.com/p> \"o\" .
<http://example.com/s> <http://example.com/p> \"o\"^^<http://example.com/d> .
<http://example.com/s> <http://example.com/p> \"o\"@en .
")

; # Basic Labels

(def basic-labels-kn
  "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
@prefix kn: <https://knotation.org/kn/>
@prefix ex: <http://example.com/>

: ex:p
rdfs:label: p

: ex:o
rdfs:label: o

: ex:s
p; kn:link: o
p: o
p; ex:d: o
p; @en: o
")

(def basic-labels-ttl
  "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix kn: <https://knotation.org/kn/> .
@prefix ex: <http://example.com/> .

ex:p
  rdfs:label \"p\" .

ex:o
  rdfs:label \"o\" .

ex:s
  ex:p ex:o ;
  ex:p \"o\" ;
  ex:p \"o\"^^ex:d ;
  ex:p \"o\"@en .
")

; # Anonymous Subjects

(def anonymous-subjects-kn
  "@prefix kn: <https://knotation.org/kn/>
@prefix ex: <http://example.com/>

: ex:s
ex:before: before
ex:a; kn:anon:
 ex:b: B
 ex:c; kn:anon:
  ex:d: D
 ex:e: E
ex:after: after
")

(def anonymous-subjects-ttl
  "@prefix kn: <https://knotation.org/kn/> .
@prefix ex: <http://example.com/> .

ex:s
  ex:before \"before\" ;
  ex:a [
    ex:b \"B\" ;
    ex:c [
      ex:d \"D\"
    ] ;
    ex:e \"E\"
  ] ;
  ex:after \"after\" .
")

; # Basic Lists

(def basic-lists-kn
  "@prefix kn: <https://knotation.org/kn/>
@prefix ex: <http://example.com/>

: ex:s
ex:p; kn:list:
 - A
 - B
 - C
")

(def basic-lists-ttl
  "@prefix kn: <https://knotation.org/kn/> .
@prefix ex: <http://example.com/> .

ex:s
  ex:p (
    \"A\"
    \"B\"
    \"C\"
  ) .
")

; # Typed Lists

(def typed-lists-kn
  "@prefix xsd: <http://www.w3.org/2001/XMLSchema#>
@prefix kn: <https://knotation.org/kn/>
@prefix ex: <http://example.com/>

: ex:s
ex:p; kn:list:
 - A
 ~ xsd:integer: 2
 ~ xsd:float: 3.0
 - D
")

(def typed-lists-ttl
  "@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix kn: <https://knotation.org/kn/> .
@prefix ex: <http://example.com/> .

ex:s
  ex:p (
    \"A\"
    \"2\"^^xsd:integer
    \"3.0\"^^xsd:float
    \"D\"
  ) .
")

; # Mixed Lists

(def mixed-lists-ttl
  "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix ex: <http://example.com/> .

ex:s
  ex:p \"before\" ;
  ex:p _:b0 ;
  ex:p [
    ex:p \"inner\" ;
    ex:p [
      ex:p \"innermost\"
    ] ;
    ex:p \"innerafter\"
  ] ;
  ex:p (
    ex:a
    ex:b
    ex:c
    ex:d
  ) ;
  ex:p (
    ex:a
    (
      ex:b
      ex:c
    )
    [
      ex:p ex:x ;
      ex:p ex:y
    ]
    ex:d
  ) ;
  ex:p \"after\" .
")

; # Basic Annotations

(def basic-annotations-kn
  "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
@prefix owl: <http://www.w3.org/2002/07/owl#>
@prefix kn: <https://knotation.org/kn/>
@prefix ex: <http://example.com/>

: ex:s
ex:p; kn:link: ex:o
> ex:a; kn:link: ex:b
")

(def basic-annotations-ttl
  "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix kn: <https://knotation.org/kn/> .
@prefix ex: <http://example.com/> .

ex:s
  ex:p ex:o .

_:b0
  rdf:type owl:Annotation ;
  owl:annotatedSource ex:s ;
  owl:annotatedProperty ex:p ;
  owl:annotatedTarget ex:o ;
  ex:a ex:b .
")

; # Nested Annotations

(def nested-annotations-kn
  "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
@prefix owl: <http://www.w3.org/2002/07/owl#>
@prefix ex: <http://example.com/>

: ex:s
ex:a: A
> ex:b: B
>> ex:c: C
> ex:d: D
")

(def nested-annotations-ttl
  "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix ex: <http://example.com/> .

ex:s
  ex:a \"A\" .

_:b0
  rdf:type owl:Annotation ;
  owl:annotatedSource ex:s ;
  owl:annotatedProperty ex:a ;
  owl:annotatedTarget \"A\" ;
  ex:b \"B\" .

_:b1
  rdf:type owl:Annotation ;
  owl:annotatedSource _:b0 ;
  owl:annotatedProperty ex:b ;
  owl:annotatedTarget \"B\" ;
  ex:c \"C\" .

_:b2
  rdf:type owl:Annotation ;
  owl:annotatedSource ex:s ;
  owl:annotatedProperty ex:a ;
  owl:annotatedTarget \"A\" ;
  ex:d \"D\" .
")
