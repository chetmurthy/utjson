
# Introduction

This project implements a type system for JSON, which one could describe as:

* directly inspired by JSON Schema, and pretty much 100% compatible
  with JSON Schema, in the sense that there is a converter from JSON
  Schema to this type system, and it is evidently straightforward to
  convert back.
  
* based on union typing, as I remember it from the school of
  Dezani-Ciancaglini, Barbanera, Berardi, and others at the
  Dipartimento di Informatica at the University of Torino.  I can't
  find any open-source publications of their work, b/c it was so long
  ago that it was all published thru places like Springer, but here's
  a reference I found to an LNCS volume (of TACS 1991):
  
  Title: Intersection and union types
  Authors: Franco BarbaneraMariangiola Dezani-Ciancaglini
  LNCS 526
  
I'll present this type system as if it's unrelated to JSON Schema, but
at various critical points I'll gesture at JSON Schema, so it's clear
what various bits are used for.

# Motivation

## What's Wrong with JSON Schema?

Why do this?  Why not just JSON Schema?  What's wrong with JSON Schema?

* ungainly, verbose, b/c expressed in JSON

* Weird syntax corner-cases in JSON Schema: here-and-there are weird
  syntax bit stuffed into corners, instead of using already-existing
  mechanisms.
  
  An example: the "type" field of a schema is typicalyl a string, but
  can be an array of strings, e.g. `"type": ["string", "object"]`.
  And then, there can be `"properties"`, but those properties
  evidently will not apply if the type of the value being validated is
  a `string`.  It would be clearer if instead, a `anyOf` were used,
  with `string` and `object` (with properties) as the two
  possibilities. But that would be much, much more verbose, and so
  JSON Schema chose to put in this weird shortcut.
  
  Another example: `$defs` is supposed to be an `object` with k/v
  pairs, the value being a schema.  But this is routinely violated,
  and we find `$defs` that are just objects with other objects, and
  somewhere beneath, there are k/v pairs denoting schema.
  
  Another: In a schema that describes an object, the properties are
  supposed to be mentioned under a `properties` key, but sometimes
  they're just mentioned directly.
  
  All of these have as a goal to reduce the verbosity of the schema.
  But if we had a targetd human-readable front-end language, we could
  arrange for (e.g.) `anyOf` to be syntactically trivial, ditto
  listing properties.
  
* Too high-level: JSON Schema has `$defs`, `$ref`, `$dynamicRef`,
  maybe other stuff, that makes it difficult for implementors.  What
  might be nice, is if this high-level version of JSON Schema, were
  *compiled down* to some lower-level representation, maybe itself a
  form of JSON Schema, that eschewed all these higher-level bits, and
  admitted of a straightforward implementation of validators.

There are probably other issues, but I'll stop here.

## Non-Goals

This type system is *not* designed to be different from JSON Schema.
The idea is that instead of writing a JSON Schema, you would write one
of these, and if you needed one, you could generate a JSON Schema.  Of
course, there's a "converter" that converts (most) JSON Schema to this
type system.  I expect that most JSON Schema will convert; when some
do not, I will provide clear arguments for why those Schema are
problematic.

The obvious observation when looking at JSON and typing it, is that
since there is so much JSON out there, you have to “meet them where
they are” and not impose constraints a priori.  So something like “map
ML types to JSON” won’t work.  Instead, I’m going to propose using
“union typing”.  This is a system where types represent “constraints”
on values, and one can both union (“||”), intersect (“&&”) and
exclusive-or (“xor”) type-constraints.

## Goals

* Be as close as possible to JSON Schema, covering as many of the
  types as JSON Schema covers.  That is to say, as much as possible,
  if we think of a JSON Schema as being equivalent to the set of JSON
  documents it validates, then as much as possible, we want to have
  our type system cover the same sets of JSON documents, as JSON
  Schema covers

* The type expressions should enjoy as many algebraic equalities as
  possible -- associative, commutative, distributive laws should hold
  for type-expressions.  It will become clear why this is useful, when
  it comes time to do generate low-level schema, from which we can
  efficiently validate JSON values.
  
* Compile down to a low-level schema, which can be used as input to
  schema-validation in many other language implementations (which
  hence won't have to understand the full complexity of JSON Schema,
  but only the subset -- a sort of machine code).
  
* Everywhere possible, borrow from programming-language type systems,
  which have demonstrated their ability to represent large sets of
  types without confusing developers.

# The Type System

* `utype`s: The type system revolves around defining "union types",
  herein abbreviated *utype*s.  In this text, we'll sometimes call
  them constraints, for reasons that should become clear.

* `structure`s: A `structure` is a collection of named `utype`s, as in
  the style of OCaml modules.

* `signature`s: a `signature` is the "type" of a `structure` -- that
  is to say, it describes the types that that structure exports.
  
* In order to implement the JSON Schema notion of
  "dynamicAnchor"/"dynamicRef" in a sensible way, we also have
  "functors" (again from OCaml) which are functions from one structure
  to another.  That is to say, they are parameterized structures.
  I'll show how the "tree"/"strictTree" example can be redone, and
  hopefully some others.

## Atomic `utype`s

* The simplest utypes are the raw types of JSON:
  ```
  type t = object ;
  ```
  [also, `null`, `bool`, `string`, `number`, `array`]

	It should be obvious when a JSON value is validated by such utypes.

* Next are "constraints".  "<fieldname>" here is a quoted-string, as
  in JSON Schema.

  * fields:
  
    `[ <fieldname>: <constraint-utype> ; ]`
  
	(equivalent to `properties`)
  
	`[ /<field-name-pcre-compat-regexp>/: <constraint-utype> ; ]`

	(equivalent to `patternProperties`)

    `[ required: <field-name> ; ]`

	As I’ll describe later, these imply the constraint “object”.

  Multiple constraints can be strung together, viz.

  `[ "a": object ; "b": array; required "a", "b" ]`

  Semicolons are separators, but a final semicolon is allowed.

  * arrays

    `[ of <constraint-utype> ; ]`

	same as `"items"` with a schema argument.

    `[ <constraint-utype1> * <constraint-utype2> * <constraint-utype3> ; ] (etc)`

	same as `"items"` with a list of schema.

    `[ unique ; ]`

	same as `"uniqueItems"`

    `[ size <range-constraint> ; ]`

	range-constraints are explained below.

    `[ <index-int>: <constraint-utype> ; ]`

	This means that the <index-int>-th value (zero-based) satisfies <constraint-utype>.

	`<range-constraint>` is as in mathematics, viz. `[0,4)`, `(0,3)`,
    `(1,4]` etc with the customary meaning that square-brackets mean
    inclusive bound and parens mean exclusive bound.  For an
    unconstrained upper bound, use “...” (in which case
    inclusive/exclusive is meaningless).  Here numbers are interpreted
    as integers.

  * strings

	`[ size <range-constraint> ; ]`
	`[ /<pcre-compatible-regexp>/ ]`

  * numbers

	`[ bounds <range-constraint> ; ]`

	Upper and lower bounds can be .inf or -.inf, and since JSON
    doesn’t allow those, clearly only exclusive bounds work with
    those.  Here numbers are integers or floats.

  * sealing an object:

	An object can be sealed

	`[ sealed ; ]`

	or its otherwise-unvalidated fields can be given a default
    constraint

	`[ orelse <constraint> ; ]`

	An object that neither given “sealed” nor “orelse” constraint is
    implicitly given an “unsealed” constraint.  This gets introduced
    during schema-processing.

## Composite `utype`s

Constraints can be *composed* using conjunction and disjunction:

`<con1> && <con2>`: JSON values satisfying this constraint satisfy both <con1> and <con2>.
`<con1> || <con2>`: JSON values satisfying this constraint satisfy either or both of <con1>, <con2>

Similarly there is 

`<con1> xor <con2>`:  (AKA “oneOf”), which is like “or”, but only one side can be satisfied.

`not <con1>`: which is satisfied exactly when the constraint <con1> is not satisfied.

`<con1> => <con2>`: the same as "if-then" in JSON Schema: an implication.

## Examples

`string && [ size (0..26] ; ] a nonempty string of max length 26.

utypes between square-brackets that are &&-ed (conjoined) can be merged, viz.

`[ “name”: string ; ] && [ “age”: number ; ]` is the same as
`[ “name”: string ; “age”: number ; ]`

## Naming constraints

utypes can be named, viz.
```
type c1 = string ;
type c2 = c1 && [ size [0,26) ; ] ;
```
utypes are only visible after having been declared, but can be declared as part of a recursive group:
```
type rec c1 = …
and c2 = ….
and c3 = ….
;
```
## Modules and Signatures

utype-decls can be grouped in “modules”, viz.
```
module M = struct
<utype-decls-terminated-by-semicolon>
end ;
```
A utype `c2` in module `M` is named as `M.c2` outside that module.

By default, declared constraints are exported.  But via local-declaration, they can be declared for use, but not exported:

`local <constraint-declarations> in <constraint-declarations>;`

Constraints can be “imported” from HTTP URLs, viz

`import <url> as M ;`

Which is the same as

`module M = struct <contents of url inserted here> end ;`

And finally, a module can be “opened”, so that its contents are usable without the module-prefix:

`open M;`

