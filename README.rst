=====================
Union Typing for JSON
=====================

.. contents::
  :local:


Introduction
============

This project implements a type system for JSON (Union Typing for JSON,
or UTJ), which one could describe as:

-  directly inspired by JSON Schema, and pretty much 100% compatible
   with JSON Schema, in the sense that there is a converter from JSON
   Schema to this type system, and it is evidently straightforward to
   convert back.

-  based on union typing, as I remember it from the school of
   Dezani-Ciancaglini, Barbanera, Berardi, and others at the
   Dipartimento di Informatica at the University of Torino. I can't find
   any open-source publications of their work, b/c it was so long ago
   that it was all published thru places like Springer, but here's a
   reference one of the authors suggested as being useful::

     Intersection and Union Types: Syntax and Semantics
     Barbanera, F., Dezani-Ciancaglini, M. and Deliguoro, U.
     Information and Computation Volume 119, Issue 2, June 1995, Pages 202-230

     https://www.sciencedirect.com/science/article/pii/S0890540185710863

I'll present this type system as if it's unrelated to JSON Schema, but
at various critical points I'll gesture at JSON Schema, so it's clear
what various bits are used for.

UTJ by Example
==============

In this section, I'll reproduce the examples found in
`JSON Schema: Getting Started Step-By-Step <https://json-schema.org/learn/getting-started-step-by-step>`_,
but using UTJ.

A Simple Product Schema
-----------------------

Suppose we want to give a type for a product-catalog entry::

  {
    "productId": 1,
    "productName": "A green door",
    "price": 12.50,
    "tags": [ "home", "green" ]
  }

Here is a schema in UTJ::

  type nonrec t = object && [
    "productId": integer;
    "productName": string;
    "price": number && [ bounds (0.,max] ];
    "tags": array && [ of string ; unique ; size [1,max] ];
    required "productId", "productName", "price"
  ] ;

Some notes:

- you can compare this to `the original example <https://json-schema.org/learn/getting-started-step-by-step#intro>`_,
  to see how the various "type constraints" are equivalent to various bits of JSON Schema. 

- as you'd expect, each field has a (union) type.

- the type `integer` is defined in a `Predefined.utj` file that is
  automatically imported (we'll see how to do that soon):;

  type integer = number && [ multipleOf 1.0 ] ;
  type scalar = boolean || number || string ;
  type json = null || scalar || array || object ;
  type positive_number = number && [ bounds (0.,max] ] ;

- we can think of a union type ("utype") as a logical expression:

  - in the first example, it's all conjunctions (`&&`), so the "price"
    field is "a number AND bounded to be positive"
  - a value of type `t` is an object AND has a field "productID" (of
    such-and-such type) AND has a field "productName", AND various
    fields are required.
  - in `Predefined.utj`, we can see disjunctions (`||`) as well as
    conjunctions.  Later we'll see exclusive-or (`xor`), not (`not`)
    and implication (`=>`); the normal precedence rules hold for
    these, and the type system admits the normal associative,
    commutative, and distributive laws.  These are useful for
    rewriting the type-expressions into a lower-level form suitable
    for validation.

Add a field with a Complex Schema Type
--------------------------------------

Now we want to add "dimensions" and "warehouseLocation" keys to the product::

  {
    "productId": 1,
    "productName": "An ice sculpture",
    "price": 12.50,
    "tags": [ "cold", "ice" ],
    "dimensions": {
      "length": 7.0,
      "width": 12.0,
      "height": 9.5
    },
    "warehouseLocation": {
      "latitude": -78.75,
      "longitude": 20.4
    }
  }

For "dimensions", we just add another field::

  type nonrec t = object && [
    "productId": integer;
    "productName": string;
    "price": number && [ bounds (0.,max] ];
    "tags": array && [ of string ; unique ; size [1,max] ];
    required "productId", "productName", "price"
    "dimensions": object && [
        "length": number;
        "width": number;
        "height": number;
     ] && [ required "length", "width", "height" ];
  ] ;
  
Notice that the constraints for the type of "dimensions" are not in a
single "[...]" block, but spread across two conjuncts.  Internally,
`[ <c1> ; <c2> ]` is equivalent to `[ <c1> ] && [ <c2> ]`.

Import a Schema from another File
---------------------------------

Now we want to introduce a "warehouseLocation", which will be a
lattitude/longitude pair.  This is somewhat standardized, so we'd like
to refer to another schema file, `geographical-location.schema.utj`,
which contains::

  type t = object && [ "lattitude": number ; "longitude": number ] ;

And then we want to use this type in our product schema::

  local import "geographical-location.schema.utj" as GEO in
  type nonrec t = object && [
    "productId": integer;
    "productName": string;
    "price": number && [ bounds (0.,max] ];
    "tags": array && [ of string ; unique ; size [1,max] ];
    required "productId", "productName", "price"
    "dimensions": object && [
        "length": number;
        "width": number;
        "height": number;
     ] && [ required "length", "width", "height" ];
    "warehouseLocation": M0.t;
  ] ;

Wrap in a Module and Hide Some Types
------------------------------------

We could define a module and refactor types a little, viz.::

  module Product = struct
    local import "geographical-location.schema.utj" as GEO in

    type dim_t = object && [
      "length": number;
      "width": number;
      "height": number;
   ] && [ required "length", "width", "height" ] ;

    type nonrec t = object && [
      "productId": integer;
      "productName": string;
      "price": number && [ bounds (0.,max] ];
      "tags": array && [ of string ; unique ; size [1,max] ];
      required "productId", "productName", "price" ;
      "dimensions": dim_t;
      "warehouseLocation": GEO.t;
    ] ;
  end : sig type t end;

and using "signatures" (types for modules), we can ensure that the
type "dim_t" is not visible outside the module.

Seal a Type (forbid unvalidated fields)
---------------------------------------

If we wanted a version of the product that did not allow any other
fields, we could do so::

  type sealed_product_t = Product.t && [ sealed ] ;

Type "t" is nonrecursive, but if we wanted to define a recursive
type, we could do so::

  type rec t = object && [
    data : object ;
    children : array && [ of t ]
  ] ;

Use Parameterized Modules to make a Recursive Type Extensible
-------------------------------------------------------------

JSON Schema has a mechanism for extending types such as the above
(`$dynamicAnchor`, `$dynamicRef`) that is ... a little complex to
explain.  In the documentation for the AJV JSON Schema system, a`"tree"/"stricttree" example<https://ajv.js.org/guide/combining-schemas.html>`_ is given to explain how these work.
So I'll explain how to do this in UTJ.

First, the problem with the above definition is that we cannot
after-the-fact declare that no other fields are allowed in tree-nodes.
That is because even in the definition::

  type sealed_t = t && [ sealed ] ;

the constraint does not apply to the "children" field.  This is true
in JSON Schema, and it's true here, for the same reason: names refer
to things, and unless otherwise carefully specified, the things they
refer to shouldn't change.

To fix this, we can define a "parameterized module" (called a "functor")::

  module type Ext1 = sig type extension ; end ;
  module ExtensibleTree = functor( M : Ext1 ) -> struct
    type t = object && [
      "data" : object ;
      "children" : array && [ of t ]
    ] && M.extension
  end

We can apply this parameterized module::

  module StrictTree = ExtensibleTree( [ sealed ] )

Now, `StrictTree.t` is a `tree`, but it is sealed, and that is true
for all the child nodes, too.

UTJ Compared to JSON Schema
===========================

What's Wrong with JSON Schema?
------------------------------

Why do this? Why not just JSON Schema? What's wrong with JSON Schema?

-  ungainly, verbose, b/c expressed in JSON

-  Weird syntax corner-cases in JSON Schema: here-and-there are weird
   syntax bit stuffed into corners, instead of using already-existing
   mechanisms.

   - An example: in
     `ansible-inventory.json <https://github.com/SchemaStore/schemastore/blob/9deea239e5cb34e54ea71af36b1763337ad51abe/src/schemas/json/ansible-inventory.json#L12>`_
     there is a schema for a field "hosts"::

       "hosts": {
         "type": ["object", "string"],
         "patternProperties": {
           "[a-zA-Z.-_0-9]": {
             "type": ["object", "null"]
           }
         }
       },

     So "hosts" can be either a "string" or an "object".  And it can
     have fields whose names match the given pattern.  But that makes no
     sense if "hosts" is an object.  It would be possible to write this using "oneOf", viz.::

       "hosts": {
         "oneOf": [
           { "type": "string"},
           { "type": "object",
             "patternProperties": {
               "[a-zA-Z.-_0-9]": {
                 "type": ["object", "null"]
               }
             }
           }
         ]
       }

     and the same could have been done for the "type" field under
     "patternProperties".  But this verbiage makes the schema ever more
     complicated, and so I would guess that the designers tolerate the
     slight abuse of language shown above.  Instead, with UTJ, we can write::

       type hosts_t = string || (object && [ /[a-zA-Z.-_0-9]/: object || null ]) ;

     and there is no ambiguity, no abuse of language.  It's also succinct
     and comprehensible.


   - `$defs` is supposed to be an `object` where the key/value pairs are
     "type name" and schema, e.g.::

       "definitions": {
         "address": {
           "type": "object",
           "properties": {
             "street_address": { "type": "string" },
             "city":           { "type": "string" },
             "state":          { "type": "string" }
           },
           "required": ["street_address", "city", "state"]
         }
       },

But in `taskfile.json <https://github.com/SchemaStore/schemastore/blob/9deea239e5cb34e54ea71af36b1763337ad51abe/src/schemas/json/taskfile.json#L103>`_
we see that `definitions` (the old name for `$defs`) is simply a JSON object, and the typename/schema pairs are buried under another layer of objects, viz.::

    "definitions": {
    "3": {
      "env": <schema for env>,
      .... etc ....
    }
  }

  So when referencing a type "env", one uses "#/definitions/3/env".
  In
  `travis.json<https://github.com/SchemaStore/schemastore/blob/9deea239e5cb34e54ea71af36b1763337ad51abe/src/schemas/json/travis.json#L1541>`_
  we find a key/value entry under a definition with both extraneous
  JSON, and then subsidiary key/value entries::

      "definitions": {
    "nonEmptyString": <schema for "nonEmptyString">,
    "notificationObject": {
      "webhooks": <extraneous JSON>,
      "slack": <schema for "slack">,
      ... other definitions ...
    },
    "import": <schema for "import">
  },

  So to refer to type "nonEmptyString", one uses
  "#/definitions/nonEmptyString".  To refer to "import", likewise,
  "#/definitions/import".  But to refer to "slack", one uses
  "#/definitions/notificationObject/slack".

   This sort of "grouping type declarations" maps directly to modules
   in ML-like languages.

-  Ostensibly, all fields must be declared in "properties" objects.  So
   for instance, an "anyOf" should be structured thus (from `Understanding JSON Schema<https://json-schema.org/understanding-json-schema/reference/combining.html#anyof>`_::

     {
       "anyOf": [
         { "type": "string" },
         { "type": "number" }
       ]
     }

   but in `cloudify.json<https://github.com/SchemaStore/schemastore/blob/9deea239e5cb34e54ea71af36b1763337ad51abe/src/schemas/json/cloudify.json#L154>`_
   we find that the subschema of the "anyOf" lists fields but not in a "properties" object::

           "anyOf": [
	     <valid schema #1>,
             {
               "valid_values": {
                 "type": "array",
                 "items": {
                   "type": ["number", "string", "boolean", "integer"]
                 }
               }
             }
           ]



   All of these have as a goal to reduce the verbosity of the schema.
   But if we had a targetd human-readable front-end language, we could
   arrange for (e.g.) ``anyOf`` to be syntactically trivial, ditto
   listing properties.

-  Too high-level: JSON Schema has ``$defs``, ``$ref``, ``$dynamicRef``,
   maybe other stuff, that makes it difficult for implementors. What
   might be nice, is if this high-level version of JSON Schema, were
   *compiled down* to some lower-level representation, maybe itself a
   form of JSON Schema, that eschewed all these higher-level bits, and
   admitted of a straightforward implementation of validators.

There are probably other issues, but I'll stop here.

Non-Goals
---------

This type system is *not* designed to be different from JSON Schema. The
idea is that instead of writing a JSON Schema, you would write one of
these, and if you needed one, you could generate a JSON Schema. Of
course, there's a “converter” that converts (most) JSON Schema to this
type system. I expect that most JSON Schema will convert; when some do
not, I will provide clear arguments for why those Schema are
problematic.

The obvious observation when looking at JSON and typing it, is that
since there is so much JSON out there, you have to “meet them where they
are” and not impose constraints a priori. So something like “map ML
types to JSON” won't work. Instead, I'm going to propose using “union
typing”. This is a system where types represent “constraints” on values,
and one can both union (`||`), intersect (`&&`) and exclusive-or
(`xor`) type-constraints.

Goals
-----

-  Be as close as possible to JSON Schema, covering as many of the types
   as JSON Schema covers. That is to say, as much as possible, if we
   think of a JSON Schema as being equivalent to the set of JSON
   documents it validates, then as much as possible, we want to have our
   type system cover the same sets of JSON documents, as JSON Schema
   covers

-  The type expressions should enjoy as many algebraic equalities as
   possible – associative, commutative, distributive laws should hold
   for type-expressions. It will become clear why this is useful, when
   it comes time to do generate low-level schema, from which we can
   efficiently validate JSON values.

-  Compile down to a low-level schema, which can be used as input to
   schema-validation in many other language implementations (which hence
   won't have to understand the full complexity of JSON Schema, but only
   the subset – a sort of machine code).

-  Everywhere possible, borrow from programming-language type systems,
   which have demonstrated their ability to represent large sets of
   types without confusing developers.

The Type System
===============

-  `utype`: The type system revolves around defining “union types”,
   herein abbreviated *utype*. In this text, we'll sometimes call
   them constraints, for reasons that should become clear.

-  `structure`: A `structure` is a list of named
   `utype`s, as in the style of OCaml structures (the contents of a `struct....end`.

-  `signature`: a `signature` is the “type” of a `structure` –
   that is to say, it describes the types that that structure exports.

-  In order to implement the JSON Schema notion of
   “dynamicAnchor”/“dynamicRef” in a sensible way, we also have
   “functors” (again from OCaml) which are functions from one structure
   to another. That is to say, they are parameterized structures.

Atomic `utype`s
---------------

-  The simplest utypes are the raw types of JSON:

   ::

      type t = object ;

   [also, ``null``, ``bool``, ``string``, ``number``, ``array``]

   It should be obvious when a JSON value is validated by such utypes.

-  Next are “constraints”. “” here is a quoted-string, as in JSON
   Schema.

   -  fields:

      ``[ <fieldname>: <constraint-utype> ; ]``

      (equivalent to ``properties``)

      ``[ /<field-name-pcre-compat-regexp>/: <constraint-utype> ; ]``

      (equivalent to ``patternProperties``)

      ``[ required: <field-name> ; ]``

      As I'll describe later, these imply the constraint “object”.

   Multiple constraints can be strung together, viz.

   ``[ "a": object ; "b": array; required "a", "b" ]``

   Semicolons are separators, but a final semicolon is allowed.

   -  arrays

      ``[ of <constraint-utype> ; ]``

      same as ``"items"`` with a schema argument.

      ``[ <constraint-utype1> * <constraint-utype2> * <constraint-utype3> ; ] (etc)``

      same as ``"items"`` with a list of schema.

      ``[ unique ; ]``

      same as ``"uniqueItems"``

      ``[ size <range-constraint> ; ]``

      range-constraints are explained below.

      ``[ <index-int>: <constraint-utype> ; ]``

      This means that the -th value (zero-based) satisfies .

      ``<range-constraint>`` is as in mathematics, viz. \ ``[0,4)``,
      ``(0,3)``, ``(1,4]`` etc with the customary meaning that
      square-brackets mean inclusive bound and parens mean exclusive
      bound. For an unconstrained upper bound, use “…” (in which case
      inclusive/exclusive is meaningless). Here numbers are interpreted
      as integers.

   -  strings

      ``[ size <range-constraint> ; ]``
      ``[ /<pcre-compatible-regexp>/ ]``

   -  numbers

      ``[ bounds <range-constraint> ; ]``

      Upper and lower bounds can be .inf or -.inf, and since JSON
      doesn't allow those, clearly only exclusive bounds work with
      those. Here numbers are integers or floats.

   -  sealing an object:

      An object can be sealed

      ``[ sealed ; ]``

      or its otherwise-unvalidated fields can be given a default
      constraint

      ``[ orelse <constraint> ; ]``

      An object that neither given “sealed” nor “orelse” constraint is
      implicitly given an “unsealed” constraint. This gets introduced
      during schema-processing.

Composite ``utype``\ s
----------------------

Constraints can be *composed* using conjunction and disjunction:

``<con1> && <con2>``: JSON values satisfying this constraint satisfy
both and . ``<con1> || <con2>``: JSON values satisfying this constraint
satisfy either or both of ,

Similarly there is

``<con1> xor <con2>``: (AKA “oneOf”), which is like “or”, but only one
side can be satisfied.

``not <con1>``: which is satisfied exactly when the constraint is not
satisfied.

``<con1> => <con2>``: the same as “if-then” in JSON Schema: an
implication.

Examples
--------

\`string && [ size (0..26] ; ] a nonempty string of max length 26.

utypes between square-brackets that are &&-ed (conjoined) can be merged,
viz.

``[ “name”: string ; ] && [ “age”: number ; ]`` is the same as
``[ “name”: string ; “age”: number ; ]``

Naming constraints
------------------

utypes can be named, viz.

::

   type c1 = string ;
   type c2 = c1 && [ size [0,26) ; ] ;

utypes are only visible after having been declared, but can be declared
as part of a recursive group:

::

   type rec c1 = …
   and c2 = ….
   and c3 = ….
   ;

Modules and Signatures
----------------------

utype-decls can be grouped in “modules”, viz.

::

   module M = struct
   <utype-decls-terminated-by-semicolon>
   end ;

A utype ``c2`` in module ``M`` is named as ``M.c2`` outside that module.

By default, declared constraints are exported. But via
local-declaration, they can be declared for use, but not exported:

``local <constraint-declarations> in <constraint-declarations>;``

Constraints can be “imported” from HTTP URLs, viz

``import <url> as M ;``

Which is the same as

``module M = struct <contents of url inserted here> end ;``

And finally, a module can be “opened”, so that its contents are usable
without the module-prefix:

``open M;``
