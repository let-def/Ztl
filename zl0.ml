let context = Z3.mk_context []

type zexpr = Z3.Expr.expr

module Order = struct type ('a, 'b) t = Lt | Eq : ('a, 'a) t | Gt end
type ('a, 'b) order = ('a, 'b) Order.t

type 'a list_ = 'a list = [] | (::) of 'a * 'a list_

[@@@ocaml.warning "-30"]

module Term = struct
  type 'a sort =
    | Boolean : boolean sort
    | Integer : integer sort
    | Real : real sort

  and 'a expr = {
    expr_z3: zexpr;
    expr_sort: 'a sort;
    expr_desc: 'a desc;
  }

  and ('dom, 'cod) symbol = {
    sym_z3: Z3.Symbol.symbol;
    sym_func_decl: Z3.FuncDecl.func_decl;
    sym_params: 'dom symbol_params;
    sym_sort: 'cod sort;
  }

  and 'a symbol_params =
    | [] : unit symbol_params
    | (::) : 'a sort * 'b symbol_params -> ('a -> 'b) symbol_params

  and 'a symbol_args =
    | [] : unit symbol_args
    | (::) : 'a expr * 'b symbol_args -> ('a -> 'b) symbol_args

  and 'a desc =
    | Expr_apply : ('dom, 'cod) symbol * 'dom symbol_args -> 'cod desc
    | Expr_if_then_else of boolean expr * 'a expr * 'a expr
    | Expr_operator of 'a
    | Expr_error of string

  and boolean =
    | Boolean_literal of bool
    | Boolean_equal : 'a expr * 'a expr -> boolean
    | Boolean_distinct : 'a expr list -> boolean
    | Boolean_not of boolean expr
    | Boolean_iff of boolean expr * boolean expr
    | Boolean_implies of boolean expr * boolean expr
    | Boolean_xor of boolean expr * boolean expr
    | Boolean_and of boolean expr list
    | Boolean_or of boolean expr list
    | Boolean_is_integer of real expr
    | Boolean_le : 'a numeral expr * 'a numeral expr -> boolean
    | Boolean_lt : 'a numeral expr * 'a numeral expr -> boolean
    | Boolean_ge : 'a numeral expr * 'a numeral expr -> boolean
    | Boolean_gt : 'a numeral expr * 'a numeral expr -> boolean

  and 'a numeral =
    | Numeral_add      of 'a numeral expr list
    | Numeral_mul      of 'a numeral expr list
    | Numeral_sub      of 'a numeral expr list
    | Numeral_minus    of 'a numeral expr
    | Numeral_div      of 'a numeral expr * 'a numeral expr
    | Numeral_power    of 'a numeral expr * 'a numeral expr
    | Numeral_specific of 'a

  and integer = integer_ numeral
  and integer_ =
    | Integer_literal of string
    | Integer_from_real of real expr
    | Integer_modulus of integer expr * integer expr
    | Integer_remainder of integer expr * integer expr

  and real = real_ numeral
  and real_ =
    | Real_literal of string
    | Real_from_int of real expr

  type 'a list = 'a list_ = [] | (::) of 'a * 'a list
end

(* Sort *)
module Sort = struct
  open Term

  let name : type a. a sort -> string = function
    | Boolean -> "boolean"
    | Integer -> "integer"
    | Real    -> "real"

  let zboolean = lazy (Z3.Boolean.mk_sort context)
  let zinteger = lazy (Z3.Arithmetic.Integer.mk_sort context)
  let zreal    = lazy (Z3.Arithmetic.Real.mk_sort context)

  let z3_sort : type a. a sort -> Z3.Sort.sort = function
    | Boolean -> Lazy.force zboolean
    | Integer -> Lazy.force zinteger
    | Real    -> Lazy.force zreal

  let compare (type a b) (a : a sort) (b : b sort) : (a, b) order =
    match a, b with
    | Boolean , Boolean -> Eq
    | Boolean , _       -> Lt
    | Integer , Integer -> Eq
    | Integer , Boolean -> Gt
    | Integer , _       -> Lt
    | Real    , Real    -> Eq
    | Real    , _       -> Gt

  module type T = sig type t val sort : t sort end
end

let z3_symbol =
  let counter = ref 0 in
  fun ?name () ->
    match name with
    | None -> incr counter; Z3.Symbol.mk_int context !counter
    | Some name -> Z3.Symbol.mk_string context name

(* Symbol *)
module Symbol = struct
  open Term

  module Arity = struct
    type 'a params = 'a symbol_params =
      | [] : unit params
      | (::) : 'a sort * 'b params -> ('a -> 'b) params
    type 'a args = 'a symbol_args =
      | [] : unit args
      | (::) : 'a expr * 'b args -> ('a -> 'b) args
  end
  type 'a params = 'a Arity.params
  type 'a args = 'a Arity.args

  let fresh (type dom cod)
      ?name (sym_params: dom params) (sym_sort: cod sort) : (dom, cod) symbol =
    let rec sorts : type a . a params -> Z3.Sort.sort list = function
      | [] -> []
      | x :: xs -> Sort.z3_sort x :: sorts xs
    in
    let zparams = sorts sym_params in
    let zsort = Sort.z3_sort sym_sort in
    let sym_z3 = z3_symbol ?name () in
    let sym_func_decl = Z3.FuncDecl.mk_func_decl context sym_z3 zparams zsort in
    { sym_z3; sym_func_decl; sym_params; sym_sort }

  let dom s = (s : _ symbol).sym_params
  let cod s = (s : _ symbol).sym_sort
  (*let compare s1 s2 =*)
end

module Expr = struct
  open Term

  type 'a desc = 'a Term.desc =
    | Expr_apply : ('dom, 'cod) symbol * 'dom symbol_args -> 'cod desc
    | Expr_if_then_else of boolean expr * 'a expr * 'a expr
    | Expr_operator of 'a
    | Expr_error of string

  let zexpr x = x.expr_z3

  let apply (type dom cod)
      (sym : (dom, cod) symbol) (args : dom Symbol.args) : cod expr =
    let rec zargs : type a . a Symbol.args -> zexpr list = function
      | [] -> []
      | x :: xs -> x.expr_z3 :: zargs xs
    in
    let expr_z3 = Z3.FuncDecl.apply sym.sym_func_decl (zargs args) in
    { expr_z3; expr_sort = sym.sym_sort; expr_desc = Expr_apply (sym, args) }

  let desc x = x.expr_desc

  let lift1 op e1 = op context (zexpr e1)
  let lift2 op e1 e2 = op context (zexpr e1) (zexpr e2)
  let liftl op es = op context (List.map zexpr es)

  let boolean_operator : boolean -> zexpr = function
    | Boolean_literal b        -> Z3.Boolean.mk_val context b
    | Boolean_equal (e1, e2)   -> lift2 Z3.Boolean.mk_eq e1 e2
    | Boolean_distinct es      -> liftl Z3.Boolean.mk_distinct es
    | Boolean_not e1           -> lift1 Z3.Boolean.mk_not e1
    | Boolean_iff (e1, e2)     -> lift2 Z3.Boolean.mk_iff e1 e2
    | Boolean_implies (e1, e2) -> lift2 Z3.Boolean.mk_implies e1 e2
    | Boolean_xor (e1, e2)     -> lift2 Z3.Boolean.mk_xor e1 e2
    | Boolean_and es           -> liftl Z3.Boolean.mk_and es
    | Boolean_or  es           -> liftl Z3.Boolean.mk_or es
    | Boolean_is_integer e1    -> lift1 Z3.Arithmetic.Real.mk_is_integer e1
    | Boolean_le (e1, e2)      -> lift2 Z3.Arithmetic.mk_le e1 e2
    | Boolean_lt (e1, e2)      -> lift2 Z3.Arithmetic.mk_lt e1 e2
    | Boolean_ge (e1, e2)      -> lift2 Z3.Arithmetic.mk_ge e1 e2
    | Boolean_gt (e1, e2)      -> lift2 Z3.Arithmetic.mk_gt e1 e2

  let numeral_operator (base : 'a -> zexpr) : 'a numeral -> zexpr = function
    | Numeral_add el -> liftl Z3.Arithmetic.mk_add el
    | Numeral_mul el -> liftl Z3.Arithmetic.mk_mul el
    | Numeral_sub el -> liftl Z3.Arithmetic.mk_sub el
    | Numeral_minus e1 -> lift1 Z3.Arithmetic.mk_unary_minus e1
    | Numeral_div   (e1, e2) -> lift2 Z3.Arithmetic.mk_div e1 e2
    | Numeral_power (e1, e2) -> lift2 Z3.Arithmetic.mk_power e1 e2
    | Numeral_specific num -> base num

  let integer_operator : integer_ -> zexpr = function
    | Integer_literal str -> Z3.Arithmetic.Integer.mk_numeral_s context str
    | Integer_from_real e1 -> lift1 Z3.Arithmetic.Real.mk_real2int e1
    | Integer_modulus   (e1, e2) -> lift2 Z3.Arithmetic.Integer.mk_mod e1 e2
    | Integer_remainder (e1, e2) -> lift2 Z3.Arithmetic.Integer.mk_rem e1 e2

  let real_operator : real_ -> zexpr = function
    | Real_literal str -> Z3.Arithmetic.Real.mk_numeral_s context str
    | Real_from_int e1 -> lift1 Z3.Arithmetic.Integer.mk_int2real e1

  let operator (type a) (expr_sort : a sort) (op : a) : a expr =
    let expr_z3 = match expr_sort with
      | Boolean -> boolean_operator op
      | Integer -> numeral_operator integer_operator op
      | Real    -> numeral_operator real_operator op
    in
    { expr_z3; expr_sort; expr_desc = Expr_operator op }

  let if_ cond ~then_ ~else_ =
    let expr_z3 =
      Z3.Boolean.mk_ite context (zexpr cond) (zexpr then_) (zexpr else_)
    in
    { expr_z3;
      expr_sort = then_.expr_sort;
      expr_desc = Expr_if_then_else (cond, then_, else_) }

  let of_bool  b1    = operator Boolean (Boolean_literal b1)
  let equal    b1 b2 = operator Boolean (Boolean_equal (b1, b2))
  let distinct bs    = operator Boolean (Boolean_distinct bs)
  let not      b1    = operator Boolean (Boolean_not b1)
  let iff      b1 b2 = operator Boolean (Boolean_iff (b1, b2))
  let implies  b1 b2 = operator Boolean (Boolean_implies (b1, b2))
  let xor      b1 b2 = operator Boolean (Boolean_xor (b1, b2))
  let and_     bs    = operator Boolean (Boolean_and bs)
  let or_      bs    = operator Boolean (Boolean_or bs)

  let add      x1 x2 = operator x1.expr_sort (Numeral_add [x1; x2])
  let multiply x1 x2 = operator x1.expr_sort (Numeral_mul [x1; x2])
  let subtract x1 x2 = operator x1.expr_sort (Numeral_sub [x1; x2])
  let negate   x1    = operator x1.expr_sort (Numeral_minus x1)
  let divide   x1 x2 = operator x1.expr_sort (Numeral_div (x1, x2))
  let power    x1 x2 = operator x1.expr_sort (Numeral_power (x1, x2))

  let le x1 x2 = operator Boolean (Boolean_le (x1, x2))
  let lt x1 x2 = operator Boolean (Boolean_lt (x1, x2))
  let ge x1 x2 = operator Boolean (Boolean_ge (x1, x2))
  let gt x1 x2 = operator Boolean (Boolean_gt (x1, x2))

  let of_integer str = operator Integer (Numeral_specific (Integer_literal str))
  let integer_sum         ints = operator Integer (Numeral_add ints)
  let integer_subtraction ints = operator Integer (Numeral_sub ints)
  let integer_product     ints = operator Integer (Numeral_mul ints)

  let modulus   x1 x2 = operator Integer (Numeral_specific (Integer_modulus (x1, x2)))
  let remainder x1 x2 = operator Integer (Numeral_specific (Integer_remainder (x1, x2)))

  let of_real str = operator Real (Numeral_specific (Real_literal str))
  let real_sum         reals = operator Real (Numeral_add reals)
  let real_subtraction reals = operator Real (Numeral_sub reals)
  let real_product     reals = operator Real (Numeral_mul reals)
end

module Infix = struct
  open Expr

  let ( ==? )  = equal
  let ( !? )   = not
  let ( <=>?)  = iff
  let ( ==>? ) = implies
  let ( &&? )  e1 e2 = and_ [e1; e2]
  let ( ||? )  e1 e2 = or_ [e1; e2]

  let ( +? ) = add
  let ( *? ) = multiply
  let ( -? ) = subtract
  let ( ~-?) = negate
  let ( /? ) = divide
  let ( ^? ) = power

  let ( % ) = apply

  let ( <=? ) = le
  let ( <? )  = lt
  let ( >=? ) = ge
  let ( >? )  = gt
end

module Context = struct
  open Term

  type t =
    | Empty
    | Asserts of Z3.Solver.solver lazy_t * boolean expr list

  let empty : t = Empty

  let split = function
    | Empty -> lazy (Z3.Solver.mk_simple_solver context), []
    | Asserts (solver, exprs) -> (solver, exprs)

  let assert_ exprs t =
    let solver, exprs' = split t in
    let solver = lazy (
      let lazy solver = solver in
      Z3.Solver.add solver (List.map Expr.zexpr exprs);
      solver
    ) in
    Asserts (solver, exprs @ exprs')

  let check t exprs : [`Sat | `Unsat | `Unknown] =
    match t, exprs with
    | Empty, [] -> `Sat
    | _ ->
      let lazy solver, _ = split t in
      match Z3.Solver.check solver (List.map Expr.zexpr exprs) with
      | Z3.Solver.UNSATISFIABLE -> `Unsat
      | Z3.Solver.UNKNOWN       -> `Unknown
      | Z3.Solver.SATISFIABLE   -> `Sat
end
