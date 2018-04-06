# Knotation: A notation for knowledge representation

[![Build Status](https://travis-ci.org/knotation/knotation-cljc.svg?branch=master)](https://travis-ci.org/knotation/knotation-cljc)

Knotation is a text format for sharing knowledge between people and machines.
Here you'll find the reference implementation,
written in [Clojure](https://clojure.org)
and designed to run on both the Java Virtual Machine and JavaScript runtime.

**This is alpha code!**


## Example

TODO


## Usage

TODO:

- from JavaScript
- from Clojure
- from Java


## Design

Knotation is a concrete syntax for RDF. We represent an RDF quad(ruple) as a Clojure map with keys:

- `::rdf/graph`
- `::rdf/subject`
- `::rdf/predicate`
- `::rdf/object`

These keys can take values that are also represented by maps, and they have the following keys:

- IRI (a name): `::rdf/iri`
- blank node (anonymous): `::rdf/blank`
- plain literal: `::rdf/lexical`
- language literal: `::rdf/lexical`, `::rdf/language`
- typed literal: `::rdf/lexical`, `::rdf/datatype`

RDF can encode compound data structures such as lists and OWL logical expressions as sub-trees using blank nodes. We prefer to collapse these 'branch' structures into the `object` using the `::rdf/pairs` key, with a vector of predicate-object nodes.

Knotation statement lines correspond to RDF quads. Knotation also includes declaration, subject, comment, and blank lines. We define a `state` map that can represent any Knotation line. The state map for a Knotation statement includes the same data as an RDF quad. States can also encode errors.

One key advantage over other RDF syntaxes is that Knotation allows prefixed names and labels in place of IRIs. In order to convert a prefixed name or label to an IRI, we need an environment. The environment is a map-of-maps from prefix to expansion, and from label to IRI.

Knotation is read line-by-line. To read a line we require an environment. The result is a state. Using the state, we can update the environment.

Knotation is also written line-by-line. To write a line we need a state and the current environment. The environment tells us which prefixed names and labels are available. Think of this line-by-line processing as a zipper (the fastening device, not the datastructure), switching back and forth between environment and state, one step at a time.


## License

Copyright © 2018 Knocean, Inc.

Distributed under the BSD 3-Clause License.
