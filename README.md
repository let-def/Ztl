# Z3 Type Layer

[Z3](https://github.com/Z3Prover/z3) is a powerful tool for working with first-order logic however the [OCaml API](https://z3prover.github.io/api/html/ml/index.html) is quite hard to work with: it is mostly untyped, all terms are represented by `Z3.Expr.expr`, even though they might have very different types in the logic.

`Ztl` implements a strongly typed layer on top of Z3 ML Api. The type system of Z3 logic as well as the syntax of sorts and terms are encoded quite faithfully in OCaml type system.

You might also be interested in [Z3overlay](https://github.com/termite-analyser/z3overlay/), another typed interface to Z3.

## Limitations

As of today the following Z3 sorts are supported: boolean, integers, reals, arrays, bitvectors and sets.

Only simple satisfiability checking is implemented. Advanced operations such as simplification, optimization and model extraction might be implemented in the future.

## Installation

Assuming you have a working opam/ocaml installation:

```shell
opam install ztl
```

## Ztl Guide

Here is a translation of a few examples from the [Z3 Guide](https://rise4fun.com/z3/tutorialcontent/guide). Follow the explanation from the guide and use the code examples in an OCaml toplevel.

To setup the toplevel, after installing ztl:

```ocaml
$ utop
# #require "ztl";;
# open Ztl;;
```

### Basic Commands

```ocaml
let a = Symbol.const Integer
(* val a : (unit, integer) symbol = <abstr> *)
let f = Symbol.fresh [Integer; Boolean] Integer
(* val f : (integer -> boolean -> unit, integer) symbol = <abstr> *)
let p1 = Numeral.gt (Term.const a) (Integer.of_int 10)
(* val p1 : boolean term = <abstr> *)
let p2 =
  Numeral.lt
    (Term.apply f [Term.const a; Boolean.of_bool true]) 
    (Integer.of_int 100)
(* val p2 : boolean term = <abstr> *)
let sat = Context.check [p1; p2]
(* val sat : [ `Impossible | `Possible | `Unknown ] = `Possible *)
```

- Z3 functions are declared using `Symbol.fresh domain codomain`.
- A constant is a function that takes no argument, `Symbol.const sort == Symbol.fresh [] sort`.
- Z3 Sorts are reflected in OCaml values and OCaml types:
  - `(integer -> boolean -> unit, integer) symbol` is the type of a function to integers that is parameterized by an integer and a boolean 
  - `(unit, integer) symbol` is the symbol of an integer constant
- `'a term` is the type of a Z3 term of sort `'a`. The encoding of Z3 sorts in OCaml type system guarantees that only well-sorted terms can be built.
- In Z3 native language, assertions are accumulated as a side-effect and  satisfiability is checked with the `(check-sat)`  command.
  In `Ztl`, terms are built, and checked for satisfiability in a context without implicit state. The API strives to be pure.

A few syntactic sugars are available in the `Infix` module:

```ocaml
open Infix
let a = Symbol.const Integer
(* val a : (unit, integer) symbol = <abstr> *)
let f = Symbol.fresh [Integer; Boolean] Integer
(* val f : (integer -> boolean -> unit, integer) symbol = <abstr> *)
let p1 = (a%[]) >? (Integer.of_int 10)
(* val p1 : boolean term = <abstr> *)
let p2 =
  (f%[a%[]; Boolean.of_bool true]) <? (Integer.of_int 100)
(* val p2 : boolean term = <abstr> *)
let sat = Context.check [p1; p2]
(* val sat : [ `Impossible | `Possible | `Unknown ] = `Possible *)
```

- operators lifted to the logic generally have a `?` appended:
  - arithmetic: `+?`, `-?`, `*?`, `/?`, ...
  - comparison: `<>?`, `==?`, ...
  - ordering of numbers: `<?`, `>?`, ...
  - boolean: `||?`, `&&?`, `<=>?`, ...
  - implication is ` @==>` to get the right associativity ([OCaml precedence and associativity](https://caml.inria.fr/pub/docs/manual-ocaml/expr.html#sec133))
- symbol application is done with `%`:
  - functional are applied with `f%[t1; t2]`
  - constants are applied with `a%[]`

TODO: lighter syntax for application?!

TODO: it is not possible to retrieve the model yet from Ztl

### Propositional Logic

```ocaml
let p = Symbol.const Boolean
let q = Symbol.const Boolean
let r = Symbol.const Boolean
(* val p : (unit, boolean) symbol = <abstr>
   val q : (unit, boolean) symbol = <abstr>
   val r : (unit, boolean) symbol = <abstr> *)
let conjecture =
    ((p%[] @==> q%[]) &&? (q%[] @==> r%[]))
  @==>
    p%[] @==> r%[];;
(* val conjecture : boolean term = <abstr> *)
let sat = Context.check [Boolean.not conjecture]
(* val sat : [ `Impossible | `Possible | `Unknown ] = `Impossible *)

```

#### Satisfiability and Validity

```ocaml
let a = Symbol.const Boolean
let b = Symbol.const Boolean
let not_ = Boolean.not
let demorgan = (a%[] &&? b%[]) ==? not_ (not_ (a%[]) ||? not_ (b%[]))
let sat = Context.check [not_ demorgan]
(* val sat : [ `Impossible | `Possible | `Unknown ] = `Impossible *)
```

