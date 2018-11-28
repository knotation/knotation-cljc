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

### Testing

To run *most* tests, use `lein test`. This will test everything in `test/org/knotation`, except the `cljs` directory.

The `cljs` directory contains the test for `cljs_api.cljs`, which does not run on the JVM. Instead, it is compiled to JavaScript then run on a server using [Karma](https://karma-runner.github.io/latest/index.html). This is all managed with the [doo](https://github.com/bensu/doo) plugin.

To run the `doo` tests, first you need to set up the Karma environment. In the root directory of this project, first install the Karma plugins:
```
npm install karma --save-dev
```
Then install the ClojureScript plugin for Karma:
```
npm install karma karma-cljs-test --save-dev
```
You will also need the Karma CLI tool:
```
npm install -g karma-cli
```
And finally, the launcher for the test environment:
```
npm install [LAUNCHER] --save-dev
```

Replace `[LAUNCHER]` with the Karma launcher you'd like to test with:
* Chrome: `karma-chrome-launcher`
* Firefox: `karma-firefox-launcher`
* Safari: `karma-safari-launcher`
* Opera: `karma-opera-launcher`
* IE: `karma-ie-launcher`

Once you have the necessary dependencies, you can test the `cljs` directory contents using:
```
lein doo [ENV] test
```

Replace `[ENV]` with the launcher you downloaded:
* Chrome: `chrome` or `chrome-headless`
* Firefox: `firefox` or `firefox-headless`
* Safari: `safari` (requires browser)
* Opera: `opera` (requires browser)
* IE: `ie` (requires browser)


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
