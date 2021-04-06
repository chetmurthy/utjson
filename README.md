This document describes a type system for JSON.  You could say that it’s based on union typing, but you could also say that it is based on JSON Schema, and the resemblance should become clearer as we get further in the description.  The goal here is to have a type system that is both easy to write and easy to understand.  Representing types as JSON objects is not a goal, b/c it’s so trivial once you have a mapping from the types of some programming language, to types in this system: just implement this type system in that language, and then turn a crank to get out JSON.

Introduction and Motivation

The obvious observation when looking at JSON and typing it, is that since there is so much JSON out there, you have to “meet them where they are” and not impose constraints a priori.  So something like “map ML types to JSON” won’t work.  Instead, I’m going to propose using “union typing”.  This is a system where types represent “constraints” on values, and one can both union (“||”), intersect (“&&”) and exclusive-or (“xor”) type-constraints.

Why would you want to do this?  Well, a system where we describe type-constraints as conjunctions and disjunctions, where they enjoy the standard properties (commutativity, associativity, distributivity), is one that allows us to perform optimizations on the type-expressions given, and convert them to a standard normal form that could be expressed in a form of lower-level “machine code”.  An interpreter for that machine code would type-check a JSON value, and that interpreter would be all that is needed for most implementations: the full generality of JSON Schema would not need to be accepted by most conforming implementations.  This would and should make schema validation faster and more uniform across all implementations.

Enumerate constraints

Simple constraints are:

null, string, bool, number, array, object

Fields can be constrained:

[ <fieldname>: <constraint> ; ]
[ /<fielld-name-pcre-compat-regexp>/: <constraint> ; ]
[ required: <field-name> ; ]

As I’ll describe later, these imply the constraint “object”.

Arrays can be constrained:

[ of <constraint> ; ]
[ <constraint1> * <constraint2> * <constraint3> ; ] (etc)
[ unique ; ]
[ size <range-constraint> ; ]
[ <index-int>: <constraint> ; ]
This means that the <index-int>-th value (zero-based) satisfies <constraint>

	Where <range-constraint> is as in mathematics, viz. [0,4), (0,3), (1,4] etc with the customary meaning that square-brackets mean inclusive bound and parens mean exclusive bound.  For an unconstrained upper bound, use “...” (in which case inclusive/exclusive is meaningless)

Strings can be constrained:
[ size <range-constraint> ; ]
[ /<pcre-compatible-regexp>/ ]

Numbers can be constrained:
[ bounds <range-constraint> ; ]
	Upper and lower bounds can be .inf or -.inf, and since JSON doesn’t allow those, clearly only exclusive bounds work with those.

And so on.

Sealing an object:

An object can be sealed

[ sealed ; ]

Or its unconstrained fields can be given a default constraint

[ orelse <constraint> ; ]

An object that neither given “sealed” nor “orelse” constraint is implicitly given an “unsealed” constraint.  This gets introduced during schema-processing.

Composite constraints

Constraints can be *composed* using conjunction and disjunction:

<con1> && <con2>: JSON values satisfying this constraint satisfy both <con1> and <con2>.
<con1> || <con2>: JSON values satisfying this constraint satisfy either or both of <con1>, <con2>

Similarly there is “<con1> xor <con2>”, (AKA “oneOf”), which is like “or”, but only one side can be satisfied.

There is “not <con1>”, which is satisfied exactly when the constraint <con1> is not satisfied.

NOTE WELL: We don’t have an if-then construct here, b/c it …. Isn’t what you’d want for tagged-union, and in fact, tagged-union is already properly handled by an xor of conjunctions.

Examples:

string && [ size (0..26] ; ] a nonempty string of max length 26.

Constraints between square-brackets that are &&-ed (conjoined) can be merged, viz.

[ “name”: string ; ] && [ “age”: number ; ] is the same as
[ “name”: string ; “age”: number ; ]

Naming constraints

Constraints can be named, viz.

type c1 = string ;
type c2 = c1 && [ size [0,26) ; ] ;

Constraints are only visible after having been declared, but can be declared as part of a recursive group:

type rec c1 = …
and c2 = ….
and c3 = ….
;
Constraints can be grouped in “modules”, viz.

Module M = struct
<constraint-decls-terminated-by-semicolon>
end ;

and this is a constraint-declaration. A constraint “c2” in module “M” is named as “M.c2” outside that module.

By default, declared constraints are exported.  But via local-declaration, they can be declared for use, but not exported:

local <constraint-declarations> in <constraint-declarations>;

Constraints can be “imported” from HTTP URLs, viz

Import <url> as M ;

Which is the same as

module M = struct <contents of url inserted here> end ;

And finally, a module can be “opened”, so that its contents are usable without the module-prefix:

open M;

Some Notes

xor can be mapped to or (“||”) if it can be proved that the disjuncts are mutually incompatible.  In the relevant case -- tagged unions -- this is straightforward.  It isn’t even clear that xor is useful, but leave it in, just in case.


