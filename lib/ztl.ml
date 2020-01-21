open Strong
type 'a natural = 'a Natural.t

let z3_context = Z3.mk_context []

type zexpr = Z3.Expr.expr

let z3_fresh_symbol =
  let counter = ref 0 in
  fun ?name () ->
    match name with
    | None -> incr counter; Z3.Symbol.mk_int z3_context !counter
    | Some name -> Z3.Symbol.mk_string z3_context name

type boolean   = private Sort_boolean
type _ numeral = private Sort_numeral
type integer_  = private Sort_integer
type integer   = integer_ numeral
type real_     = private Sort_real
type real      = real_ numeral
type (_, _) zarray = private Sort_zarray
type _ zset     = private Sort_zset
type _ bitvector = private Sort_bitvector

type signedness = [ `Signed | `Unsigned ]

type 'a sort =
  | Boolean : boolean sort
  | Integer : integer sort
  | Real : real sort
  | Zarray : 'a sort * 'b sort -> ('a, 'b) zarray sort
  | Zset : 'a sort -> 'a zset sort
  | Bitvector : 'a natural -> 'a bitvector sort

module Sort = struct
  let rec name : type a. a sort -> string = function
    | Boolean -> "boolean"
    | Integer -> "integer"
    | Real    -> "real"
    | Zarray (dom, cod) -> Printf.sprintf "array(%s,%s)" (name dom) (name cod)
    | Zset dom -> Printf.sprintf "zset(%s)" (name dom)
    | Bitvector dom -> Printf.sprintf "bitvector(%d)" (Natural.to_int dom)

  let compare_order a b =
    let c = compare (Obj.repr a) (Obj.repr b) in
    if c < 0 then
      Order.Lt
    else if c > 0 then
      Order.Gt
    else
      assert false

  let rec order : 'a 'b. 'a sort -> 'b sort -> ('a, 'b) order =
    fun (type a b) (a : a sort) (b : b sort) : (a, b) order ->
    match a, b with
    | Boolean , Boolean -> Eq
    | Integer , Integer -> Eq
    | Real    , Real    -> Eq
    | Zarray (a1, a2), Zarray (b1, b2) ->
      begin match order a1 b1 with
        | Eq -> begin match order a2 b2 with
            | Eq -> Eq
            | (Lt | Gt) as n -> n
          end
        | (Lt | Gt) as n -> n
      end
    | Zset a, Zset b ->
      begin match order a b with
        | Eq -> Eq
        | (Lt | Gt) as n -> n
      end
    | Bitvector a, Bitvector b ->
      begin match Natural.order a b with
        | Eq -> Eq
        | (Lt | Gt) as n -> n
      end
    | (Boolean | Integer | Real | Zarray _ | Zset _ | Bitvector _), _ ->
      compare_order a b
end

let zboolean = lazy (Z3.Boolean.mk_sort z3_context)
let zinteger = lazy (Z3.Arithmetic.Integer.mk_sort z3_context)
let zreal    = lazy (Z3.Arithmetic.Real.mk_sort z3_context)

let rec z3_sort : type a. a sort -> Z3.Sort.sort = function
  | Boolean -> Lazy.force zboolean
  | Integer -> Lazy.force zinteger
  | Real    -> Lazy.force zreal
  | Zarray (dom, cod) ->
    Z3.Z3Array.mk_sort z3_context (z3_sort dom) (z3_sort cod)
  | Zset dom -> Z3.Set.mk_sort z3_context (z3_sort dom)
  | Bitvector dom -> Z3.BitVector.mk_sort z3_context (Natural.to_int dom)

module Term = struct
  [@@@ocaml.warning "-30"]

  type 'a term = {
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

  and 'a desc =
    | Apply : ('dom, 'cod) symbol * 'dom symbol_args -> 'cod desc
    | If_then_else : boolean term * 'a term * 'a term -> 'a desc

    | Boolean_literal  : bool -> boolean desc
    | Boolean_equal    : 'a term * 'a term -> boolean desc
    | Boolean_distinct : 'a term list -> boolean desc
    | Boolean_not      : boolean term -> boolean desc
    | Boolean_iff      : boolean term * boolean term -> boolean desc
    | Boolean_implies  : boolean term * boolean term -> boolean desc
    | Boolean_xor      : boolean term * boolean term -> boolean desc
    | Boolean_and      : boolean term list -> boolean desc
    | Boolean_or       : boolean term list -> boolean desc

    | Numeral_add   : 'a numeral term list -> 'a numeral desc
    | Numeral_mul   : 'a numeral term list -> 'a numeral desc
    | Numeral_sub   : 'a numeral term list -> 'a numeral desc
    | Numeral_minus : 'a numeral term -> 'a numeral desc
    | Numeral_div   : 'a numeral term * 'a numeral term -> 'a numeral desc
    | Numeral_power : 'a numeral term * 'a numeral term -> 'a numeral desc
    | Numeral_le    : 'a numeral term * 'a numeral term -> boolean desc
    | Numeral_lt    : 'a numeral term * 'a numeral term -> boolean desc
    | Numeral_ge    : 'a numeral term * 'a numeral term -> boolean desc
    | Numeral_gt    : 'a numeral term * 'a numeral term -> boolean desc

    | Integer_literal : string -> integer desc
    | Integer_of_real : real term -> integer desc
    | Integer_modulus : integer term * integer term -> integer desc
    | Integer_remainder : integer term * integer term -> integer desc

    | Real_literal    : string -> real desc
    | Real_of_integer : integer term -> real desc
    | Real_is_integer : real term -> boolean desc

    | Zarray_const  : 'a sort * 'b term -> ('a, 'b) zarray desc
    | Zarray_select : ('a, 'b) zarray term * 'a term -> 'b desc
    | Zarray_store  : ('a, 'b) zarray term * 'a term * 'b term -> ('a, 'b) zarray desc
    | Zarray_map : ('a1 -> 'a2, 'b) symbol * ('i, 'a1 -> 'a2) symbol_array_args -> ('i, 'b) zarray desc

    | Zset_empty  : 'a sort -> 'a zset desc
    | Zset_full   : 'a sort -> 'a zset desc
    | Zset_add    : 'a term * 'a zset term -> 'a zset desc
    | Zset_remove : 'a term * 'a zset term -> 'a zset desc
    | Zset_union  : 'a zset term * 'a zset term -> 'a zset desc
    | Zset_intersect : 'a zset term * 'a zset term -> 'a zset desc
    | Zset_difference : 'a zset term * 'a zset term -> 'a zset desc
    | Zset_complement : 'a zset term -> 'a zset desc
    | Zset_is_member : 'a term * 'a zset term -> boolean desc
    | Zset_is_subset : 'a zset term * 'a zset term -> boolean desc

    | Bitvector_not : 'n bitvector term -> 'n bitvector desc
    | Bitvector_redand : 'n bitvector term -> Natural.one bitvector desc
    | Bitvector_redor : 'n bitvector term -> Natural.one bitvector desc
    | Bitvector_and  : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_or   : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_xor  : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_nand : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_nor  : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_xnor : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_neg  : 'n bitvector term -> 'n bitvector desc
    | Bitvector_add  : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_sub  : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_mul  : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_udiv : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_sdiv : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_urem : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_srem : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_smod : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_ult  : 'n bitvector term * 'n bitvector term -> boolean desc
    | Bitvector_slt  : 'n bitvector term * 'n bitvector term -> boolean desc
    | Bitvector_ule  : 'n bitvector term * 'n bitvector term -> boolean desc
    | Bitvector_sle  : 'n bitvector term * 'n bitvector term -> boolean desc
    | Bitvector_uge  : 'n bitvector term * 'n bitvector term -> boolean desc
    | Bitvector_sge  : 'n bitvector term * 'n bitvector term -> boolean desc
    | Bitvector_ugt  : 'n bitvector term * 'n bitvector term -> boolean desc
    | Bitvector_sgt  : 'n bitvector term * 'n bitvector term -> boolean desc
    | Bitvector_shl  : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_lshr : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_ashr : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_extend : signedness * 'm natural * 'n bitvector term -> 'm bitvector desc
    | Bitvector_rotate_left  : 'n bitvector term * int -> 'n bitvector desc
    | Bitvector_rotate_right : 'n bitvector term * int -> 'n bitvector desc
    | Bitvector_ext_rotate_left  : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_ext_rotate_right : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_of_integer : 'n natural * integer term -> 'n bitvector desc
    | Bitvector_to_integer : signedness * 'n bitvector term -> integer desc
    | Bitvector_add_no_overflow  : 'n bitvector term * 'n bitvector term * signedness -> 'n bitvector desc
    | Bitvector_add_no_underflow : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_sub_no_overflow  : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_sub_no_underflow : 'n bitvector term * 'n bitvector term * signedness -> 'n bitvector desc
    | Bitvector_sdiv_no_overflow : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_neg_no_overflow  : 'n bitvector term -> 'n bitvector desc
    | Bitvector_mul_no_overflow  : 'n bitvector term * 'n bitvector term * signedness -> 'n bitvector desc
    | Bitvector_mul_no_underflow : 'n bitvector term * 'n bitvector term -> 'n bitvector desc
    | Bitvector_numeral : 'n natural * string -> 'n bitvector desc
    (*| Bitvector_concat : 'n bitvector term * 'm bitvector term -> ('n, 'm) Natural.sum bitvector desc*)
    (*| Bitvector_extract : context -> int  int -> Expr.term -> Expr.term*)
    (*| Bitvector_repeat : 'n natural * 'm bitvector term -> ('n, 'm) Natural.prod bitvector desc*)

  and 'a symbol_params =
    | [] : unit symbol_params
    | (::) : 'a sort * 'b symbol_params -> ('a -> 'b) symbol_params

  and 'a symbol_args =
    | [] : unit symbol_args
    | (::) : 'a term * 'b symbol_args -> ('a -> 'b) symbol_args

  and ('i, 'a) symbol_array_args =
    | [] : ('i, unit) symbol_array_args
    | (::) : ('i, 'a) zarray term * ('i, 'b) symbol_array_args ->
        ('i, 'a -> 'b) symbol_array_args

  let zexpr x = x.expr_z3

  let zapply (type dom cod)
      (sym : (dom, cod) symbol) (args : dom symbol_args) : zexpr =
    let rec zargs : type a . a symbol_args -> zexpr list = function
      | [] -> []
      | x :: xs -> x.expr_z3 :: zargs xs
    in
    Z3.FuncDecl.apply sym.sym_func_decl (zargs args)

  let lift1 op e1 = op z3_context (zexpr e1)
  let lift2 op e1 e2 = op z3_context (zexpr e1) (zexpr e2)
  let lift3 op e1 e2 e3 = op z3_context (zexpr e1) (zexpr e2) (zexpr e3)
  let liftl op es = op z3_context (List.map zexpr es)
  let is_signed = function
    | `Signed   -> true
    | `Unsigned -> false

  let zoperator (type a) : a desc -> zexpr = function
    | Apply (sym, args) -> zapply sym args
    | If_then_else (cond, then_, else_) ->
      Z3.Boolean.mk_ite z3_context (zexpr cond) (zexpr then_) (zexpr else_)

    | Boolean_literal b        -> Z3.Boolean.mk_val z3_context b
    | Boolean_equal (e1, e2)   -> lift2 Z3.Boolean.mk_eq e1 e2
    | Boolean_distinct es      -> liftl Z3.Boolean.mk_distinct es
    | Boolean_not e1           -> lift1 Z3.Boolean.mk_not e1
    | Boolean_iff (e1, e2)     -> lift2 Z3.Boolean.mk_iff e1 e2
    | Boolean_implies (e1, e2) -> lift2 Z3.Boolean.mk_implies e1 e2
    | Boolean_xor (e1, e2)     -> lift2 Z3.Boolean.mk_xor e1 e2
    | Boolean_and es           -> liftl Z3.Boolean.mk_and es
    | Boolean_or  es           -> liftl Z3.Boolean.mk_or es

    | Numeral_add el -> liftl Z3.Arithmetic.mk_add el
    | Numeral_mul el -> liftl Z3.Arithmetic.mk_mul el
    | Numeral_sub el -> liftl Z3.Arithmetic.mk_sub el
    | Numeral_minus e1 -> lift1 Z3.Arithmetic.mk_unary_minus e1
    | Numeral_div   (e1, e2) -> lift2 Z3.Arithmetic.mk_div e1 e2
    | Numeral_power (e1, e2) -> lift2 Z3.Arithmetic.mk_power e1 e2
    | Numeral_le (e1, e2) -> lift2 Z3.Arithmetic.mk_le e1 e2
    | Numeral_lt (e1, e2) -> lift2 Z3.Arithmetic.mk_lt e1 e2
    | Numeral_ge (e1, e2) -> lift2 Z3.Arithmetic.mk_ge e1 e2
    | Numeral_gt (e1, e2) -> lift2 Z3.Arithmetic.mk_gt e1 e2

    | Integer_literal str -> Z3.Arithmetic.Integer.mk_numeral_s z3_context str
    | Integer_of_real e1 -> lift1 Z3.Arithmetic.Real.mk_real2int e1
    | Integer_modulus   (e1, e2) -> lift2 Z3.Arithmetic.Integer.mk_mod e1 e2
    | Integer_remainder (e1, e2) -> lift2 Z3.Arithmetic.Integer.mk_rem e1 e2

    | Real_literal str -> Z3.Arithmetic.Real.mk_numeral_s z3_context str
    | Real_of_integer e1 -> lift1 Z3.Arithmetic.Integer.mk_int2real e1
    | Real_is_integer e1 -> lift1 Z3.Arithmetic.Real.mk_is_integer e1

    | Zarray_const (sort, default) ->
      Z3.Z3Array.mk_const_array z3_context (z3_sort sort) (zexpr default)
    | Zarray_select   (arr, idx) -> lift2 Z3.Z3Array.mk_select arr idx
    | Zarray_store    (arr, idx, vl) -> lift3 Z3.Z3Array.mk_store arr idx vl
    | Zarray_map      (func, args) ->
      let rec zargs : type a . (_,a) symbol_array_args -> zexpr list = function
        | [] -> []
        | x :: xs -> x.expr_z3 :: zargs xs
      in
      Z3.Z3Array.mk_map z3_context func.sym_func_decl (zargs args)
    | Zset_empty      sort -> Z3.Set.mk_empty z3_context (z3_sort sort)
    | Zset_full       sort -> Z3.Set.mk_full z3_context (z3_sort sort)
    | Zset_add        (elt, zset) -> lift2 Z3.Set.mk_set_add elt zset
    | Zset_remove     (elt, zset) -> lift2 Z3.Set.mk_del elt zset
    | Zset_union      (set1, set2) -> lift2 Z3.Set.mk_subset set1 set2
    | Zset_intersect  (set1, set2) -> lift2 Z3.Set.mk_subset set1 set2
    | Zset_difference (set1, set2) -> lift2 Z3.Set.mk_subset set1 set2
    | Zset_complement set1 -> lift1 Z3.Set.mk_complement set1
    | Zset_is_member  (elt, zset) -> lift2 Z3.Set.mk_membership elt zset
    | Zset_is_subset  (set1, set2) -> lift2 Z3.Set.mk_subset set1 set2

    | Bitvector_not    t1 -> lift1 Z3.BitVector.mk_not t1
    | Bitvector_redand t1 -> lift1 Z3.BitVector.mk_redand t1
    | Bitvector_redor  t1 -> lift1 Z3.BitVector.mk_redor t1
    | Bitvector_and    (t1, t2) -> lift2 Z3.BitVector.mk_and  t1 t2
    | Bitvector_or     (t1, t2) -> lift2 Z3.BitVector.mk_or   t1 t2
    | Bitvector_xor    (t1, t2) -> lift2 Z3.BitVector.mk_xor  t1 t2
    | Bitvector_nand   (t1, t2) -> lift2 Z3.BitVector.mk_nand t1 t2
    | Bitvector_nor    (t1, t2) -> lift2 Z3.BitVector.mk_nor  t1 t2
    | Bitvector_xnor   (t1, t2) -> lift2 Z3.BitVector.mk_xnor t1 t2
    | Bitvector_neg    t1 -> lift1 Z3.BitVector.mk_neg t1
    | Bitvector_add    (t1, t2) -> lift2 Z3.BitVector.mk_add  t1 t2
    | Bitvector_sub    (t1, t2) -> lift2 Z3.BitVector.mk_sub  t1 t2
    | Bitvector_mul    (t1, t2) -> lift2 Z3.BitVector.mk_mul  t1 t2
    | Bitvector_udiv   (t1, t2) -> lift2 Z3.BitVector.mk_udiv t1 t2
    | Bitvector_sdiv   (t1, t2) -> lift2 Z3.BitVector.mk_sdiv t1 t2
    | Bitvector_urem   (t1, t2) -> lift2 Z3.BitVector.mk_urem t1 t2
    | Bitvector_srem   (t1, t2) -> lift2 Z3.BitVector.mk_srem t1 t2
    | Bitvector_smod   (t1, t2) -> lift2 Z3.BitVector.mk_smod t1 t2
    | Bitvector_ult    (t1, t2) -> lift2 Z3.BitVector.mk_ult  t1 t2
    | Bitvector_slt    (t1, t2) -> lift2 Z3.BitVector.mk_slt  t1 t2
    | Bitvector_ule    (t1, t2) -> lift2 Z3.BitVector.mk_ule  t1 t2
    | Bitvector_sle    (t1, t2) -> lift2 Z3.BitVector.mk_sle  t1 t2
    | Bitvector_uge    (t1, t2) -> lift2 Z3.BitVector.mk_uge  t1 t2
    | Bitvector_sge    (t1, t2) -> lift2 Z3.BitVector.mk_sge  t1 t2
    | Bitvector_ugt    (t1, t2) -> lift2 Z3.BitVector.mk_ugt  t1 t2
    | Bitvector_sgt    (t1, t2) -> lift2 Z3.BitVector.mk_sgt  t1 t2
    | Bitvector_shl    (t1, t2) -> lift2 Z3.BitVector.mk_shl  t1 t2
    | Bitvector_lshr   (t1, t2) -> lift2 Z3.BitVector.mk_lshr t1 t2
    | Bitvector_ashr   (t1, t2) -> lift2 Z3.BitVector.mk_ashr t1 t2
    | Bitvector_extend (sign, new_size, term) ->
      let Bitvector old_size = term.expr_sort in
      let new_size' = Natural.to_int new_size in
      let old_size' = Natural.to_int old_size in
      if new_size' = old_size' then
        zexpr term
      else if new_size' > old_size' then
        let delta = new_size' - old_size' in
        match sign with
        | `Signed   -> Z3.BitVector.mk_sign_ext z3_context delta (zexpr term)
        | `Unsigned -> Z3.BitVector.mk_zero_ext z3_context delta (zexpr term)
      else
        invalid_arg "Zl0.Bitvector.extend: vector is smaller"
    | Bitvector_rotate_left  (vector, amount) ->
      Z3.BitVector.mk_rotate_left z3_context amount (zexpr vector)
    | Bitvector_rotate_right (vector, amount) ->
      Z3.BitVector.mk_rotate_right z3_context amount (zexpr vector)
    | Bitvector_ext_rotate_left  (vector, amount) ->
      lift2 Z3.BitVector.mk_ext_rotate_left vector amount
    | Bitvector_ext_rotate_right (vector, amount) ->
      lift2 Z3.BitVector.mk_ext_rotate_left vector amount
    | Bitvector_of_integer (w, x) ->
      Z3.Arithmetic.Integer.mk_int2bv z3_context (Natural.to_int w) (zexpr x)
    | Bitvector_to_integer (sign, vector) ->
      lift1 Z3.BitVector.mk_bv2int vector (is_signed sign)
    | Bitvector_add_no_overflow  (t1, t2, sign) ->
      lift2 Z3.BitVector.mk_add_no_overflow t1 t2 (is_signed sign)
    | Bitvector_add_no_underflow (t1, t2) ->
      lift2 Z3.BitVector.mk_add_no_underflow t1 t2
    | Bitvector_sub_no_overflow  (t1, t2) ->
      lift2 Z3.BitVector.mk_sub_no_overflow t1 t2
    | Bitvector_sub_no_underflow (t1, t2, sign) ->
      lift2 Z3.BitVector.mk_sub_no_underflow t1 t2 (is_signed sign)
    | Bitvector_sdiv_no_overflow (t1, t2) ->
      lift2 Z3.BitVector.mk_sdiv_no_overflow t1 t2
    | Bitvector_neg_no_overflow  t1 ->
      lift1 Z3.BitVector.mk_neg_no_overflow t1
    | Bitvector_mul_no_overflow  (t1, t2, sign) ->
      lift2 Z3.BitVector.mk_mul_no_overflow t1 t2 (is_signed sign)
    | Bitvector_mul_no_underflow (t1, t2) ->
      lift2 Z3.BitVector.mk_mul_no_underflow t1 t2
    | Bitvector_numeral (width, str) ->
      Z3.BitVector.mk_numeral z3_context str (Natural.to_int width)

  let desc x = x.expr_desc

  let make (type a) (expr_sort : a sort) (op : a desc) : a term =
    let expr_z3 = zoperator op in
    { expr_z3; expr_sort; expr_desc = op }

  let apply sym args =
    let expr_z3 = zapply sym args in
    { expr_z3; expr_sort = sym.sym_sort; expr_desc = Apply (sym, args) }

  let if_ cond ~then_ ~else_ =
    make then_.expr_sort (If_then_else (cond, then_, else_))
end

type 'a term = 'a Term.term
type ('dom, 'cod) symbol = ('dom, 'cod) Term.symbol

let z3_expr x = x.Term.expr_z3
let z3_symbol x = x.Term.sym_z3
let z3_func_decl x = x.Term.sym_func_decl

module Symbol = struct
  module Arity = struct
    type 'a params = 'a Term.symbol_params =
      | [] : unit params
      | (::) : 'a sort * 'b params -> ('a -> 'b) params
    type 'a args = 'a Term.symbol_args =
      | [] : unit args
      | (::) : 'a Term.term * 'b args -> ('a -> 'b) args
    type ('i, 'a) array_args = ('i, 'a) Term.symbol_array_args =
      | [] : ('i, unit) array_args
      | (::) : ('i, 'a) zarray term * ('i, 'b) array_args ->
          ('i, 'a -> 'b) array_args
  end

  type 'a params = 'a Arity.params
  type 'a args = 'a Arity.args
  type ('i, 'd) array_args = ('i, 'd) Arity.array_args

  let fresh (type dom cod) ?name
      (sym_params: dom params) (sym_sort: cod sort)
    : (dom, cod) Term.symbol =
    let rec sorts : type a . a params -> Z3.Sort.sort list = function
      | [] -> []
      | x :: xs -> z3_sort x :: sorts xs
    in
    let zparams = sorts sym_params in
    let zsort = z3_sort sym_sort in
    let sym_z3 = z3_fresh_symbol ?name () in
    let sym_func_decl = Z3.FuncDecl.mk_func_decl z3_context sym_z3 zparams zsort in
    { sym_z3; sym_func_decl; sym_params; sym_sort }

  let const ?name sort = fresh ?name [] sort

  let dom s = (s : _ symbol).sym_params
  let cod s = (s : _ symbol).sym_sort
end

module Boolean = struct
  open Term

  let of_bool  b1    = make Boolean (Boolean_literal b1)
  let equal    b1 b2 = make Boolean (Boolean_equal (b1, b2))
  let distinct bs    = make Boolean (Boolean_distinct bs)
  let not      b1    = make Boolean (Boolean_not b1)
  let iff      b1 b2 = make Boolean (Boolean_iff (b1, b2))
  let implies  b1 b2 = make Boolean (Boolean_implies (b1, b2))
  let xor      b1 b2 = make Boolean (Boolean_xor (b1, b2))
  let and_     bs    = make Boolean (Boolean_and bs)
  let or_      bs    = make Boolean (Boolean_or bs)
end

module Numeral = struct
  open Term

  let add      x1 x2 = make x1.expr_sort (Numeral_add [x1; x2])
  let multiply x1 x2 = make x1.expr_sort (Numeral_mul [x1; x2])
  let subtract x1 x2 = make x1.expr_sort (Numeral_sub [x1; x2])
  let negate   x1    = make x1.expr_sort (Numeral_minus x1)
  let divide   x1 x2 = make x1.expr_sort (Numeral_div (x1, x2))
  let power    x1 x2 = make x1.expr_sort (Numeral_power (x1, x2))

  let le x1 x2 = make Boolean (Numeral_le (x1, x2))
  let lt x1 x2 = make Boolean (Numeral_lt (x1, x2))
  let ge x1 x2 = make Boolean (Numeral_ge (x1, x2))
  let gt x1 x2 = make Boolean (Numeral_gt (x1, x2))
end

module Integer = struct
  open Term

  let of_string str = make Integer (Integer_literal str)
  let of_int int    = make Integer (Integer_literal (string_of_int int))
  let of_real str   = make Integer (Integer_of_real str)

  let sum       xs    = make Integer (Numeral_add xs)
  let subtract  xs    = make Integer (Numeral_sub xs)
  let product   xs    = make Integer (Numeral_mul xs)
  let modulus   x1 x2 = make Integer (Integer_modulus (x1, x2))
  let remainder x1 x2 = make Integer (Integer_remainder (x1, x2))
end

module Real = struct
  open Term

  let of_string str = make Real (Real_literal str)
  let of_integer int = make Real (Real_of_integer int)

  let sum      xs = make Real (Numeral_add xs)
  let subtract xs = make Real (Numeral_sub xs)
  let product  xs = make Real (Numeral_mul xs)

  let is_integer x = make Boolean (Real_is_integer x)
end

module Zarray = struct
  open Term

  let constant sort term =
    make (Zarray (sort, term.expr_sort)) (Zarray_const (sort, term))

  let select arr idx =
    let Zarray (_, cod) = arr.expr_sort in make cod (Zarray_select (arr, idx))

  let store arr idx v = make arr.expr_sort (Zarray_store (arr, idx, v))

  let map sym ((x :: _) as args : ('i, _ -> _) Symbol.array_args) =
    let Zarray (dom, _) = x.expr_sort in
    make (Zarray (dom, sym.sym_sort)) (Zarray_map (sym, args))
end

module Zset = struct
  open Term

  let empty sort = make (Zset sort) (Zset_empty sort)
  let full  sort = make (Zset sort) (Zset_full sort)
  let add elt set = make set.expr_sort (Zset_add (elt, set))
  let remove elt set = make set.expr_sort (Zset_remove (elt, set))
  let union set1 set2 = make set1.expr_sort (Zset_union (set1, set2))
  let intersect set1 set2 = make set1.expr_sort (Zset_intersect (set1, set2))
  let difference set1 set2 = make set2.expr_sort (Zset_difference (set1, set2))
  let complement set = make set.expr_sort (Zset_complement set)
  let is_member elt set = make Boolean (Zset_is_member (elt, set))
  let is_subset set1 set2 = make Boolean  (Zset_is_subset (set1, set2))
end

module Bitvector = struct
  open Term

  type 'n bv = 'n bitvector term
  let not  e1 = make e1.expr_sort (Bitvector_not e1)
  let and_ e1 e2 = make e1.expr_sort (Bitvector_and (e1, e2))
  let or_  e1 e2 = make e1.expr_sort (Bitvector_or (e1, e2))
  let xor  e1 e2 = make e1.expr_sort (Bitvector_xor (e1, e2))
  let nand e1 e2 = make e1.expr_sort (Bitvector_nand (e1, e2))
  let nor  e1 e2 = make e1.expr_sort (Bitvector_nor (e1, e2))
  let xnor e1 e2 = make e1.expr_sort (Bitvector_xnor (e1, e2))
  let neg  e1 = make e1.expr_sort (Bitvector_neg e1)
  let add  e1 e2 = make e1.expr_sort (Bitvector_add  (e1, e2))
  let sub  e1 e2 = make e1.expr_sort (Bitvector_sub  (e1, e2))
  let mul  e1 e2 = make e1.expr_sort (Bitvector_mul  (e1, e2))
  let udiv e1 e2 = make e1.expr_sort (Bitvector_udiv (e1, e2))
  let sdiv e1 e2 = make e1.expr_sort (Bitvector_sdiv (e1, e2))
  let urem e1 e2 = make e1.expr_sort (Bitvector_urem (e1, e2))
  let srem e1 e2 = make e1.expr_sort (Bitvector_srem (e1, e2))
  let smod e1 e2 = make e1.expr_sort (Bitvector_smod (e1, e2))
  let ult  e1 e2 = make Boolean (Bitvector_ult (e1, e2))
  let slt  e1 e2 = make Boolean (Bitvector_slt (e1, e2))
  let ule  e1 e2 = make Boolean (Bitvector_ule (e1, e2))
  let sle  e1 e2 = make Boolean (Bitvector_sle (e1, e2))
  let uge  e1 e2 = make Boolean (Bitvector_uge (e1, e2))
  let sge  e1 e2 = make Boolean (Bitvector_sge (e1, e2))
  let ugt  e1 e2 = make Boolean (Bitvector_ugt (e1, e2))
  let sgt  e1 e2 = make Boolean (Bitvector_sgt (e1, e2))
  let shl  e1 e2 = make e1.expr_sort (Bitvector_shl  (e1, e2))
  let lshr e1 e2 = make e1.expr_sort (Bitvector_lshr (e1, e2))
  let ashr e1 e2 = make e1.expr_sort (Bitvector_ashr (e1, e2))
  let redand e1 = make (Bitvector Natural.one) (Bitvector_redand e1)
  let redor  e1 = make (Bitvector Natural.one) (Bitvector_redor e1)
  let extend sign nat bv = make (Bitvector nat) (Bitvector_extend (sign, nat, bv))
  let rotate_left e1 n = make e1.expr_sort (Bitvector_rotate_left (e1, n))
  let rotate_right e1 n = make e1.expr_sort (Bitvector_rotate_right (e1, n))
  let rotate_left' e1 e2 =
    make e1.expr_sort (Bitvector_ext_rotate_left (e1, e2))
  let rotate_right' e1 e2 =
    make e1.expr_sort (Bitvector_ext_rotate_right (e1, e2))
  let of_integer w n = make (Bitvector w) (Bitvector_of_integer (w, n))
  let to_integer sign bv = make Integer (Bitvector_to_integer (sign, bv))
  let add_no_overflow e1 e2 sign =
    make e1.expr_sort (Bitvector_add_no_overflow (e1, e2, sign))
  let add_no_underflow e1 e2 =
    make e1.expr_sort (Bitvector_add_no_underflow (e1, e2))
  let sub_no_overflow  e1 e2 =
    make e1.expr_sort (Bitvector_sub_no_overflow (e1, e2))
  let sub_no_underflow e1 e2 sign =
    make e1.expr_sort (Bitvector_sub_no_underflow (e1, e2, sign))
  let sdiv_no_overflow e1 e2 =
    make e1.expr_sort (Bitvector_mul_no_underflow (e1, e2))
  let neg_no_overflow e1 =
    make e1.expr_sort (Bitvector_neg_no_overflow e1)
  let mul_no_overflow e1 e2 sign =
    make e1.expr_sort (Bitvector_mul_no_overflow (e1, e2, sign))
  let mul_no_underflow e1 e2 =
    make e1.expr_sort (Bitvector_mul_no_underflow (e1, e2))
  let numeral w str = make (Bitvector w) (Bitvector_numeral (w, str))
end

module Infix = struct
  let ( ==? )  = Boolean.equal
  (*let ( !? )   = Boolean.not*)
  let ( <=>?)  = Boolean.iff
  let ( ==>? ) = Boolean.implies
  let ( &&? ) e1 e2 = Boolean.and_ [e1; e2]
  let ( ||? ) e1 e2 = Boolean.or_ [e1; e2]

  let ( +? ) = Numeral.add
  let ( *? ) = Numeral.multiply
  let ( -? ) = Numeral.subtract
  (*let ( ~-?) = Numeral.negate*)
  let ( /? ) = Numeral.divide
  let ( ^? ) = Numeral.power

  let ( % ) = Term.apply

  let ( <=? ) = Numeral.le
  let ( <? )  = Numeral.lt
  let ( >=? ) = Numeral.ge
  let ( >? )  = Numeral.gt
end

module Context = struct
  module Solver = struct
    type state = Z3.Solver.solver
    type operation = boolean term list

    let push = Z3.Solver.push
    let pop = Z3.Solver.pop

    let apply state operations =
      Z3.Solver.add state
        (List.flatten (List.map (List.map Term.zexpr) operations))

  end
  module Tracker = State_tracker.Make(Solver)

  type t =
    | Empty
    | Tracker of Tracker.t

  let empty = Empty

  let get_tracker = function
    | Empty -> Tracker.make (Z3.Solver.mk_simple_solver z3_context)
    | Tracker t -> t

  let assert_ terms t =
    Tracker (Tracker.apply terms (get_tracker t))

  let check ?(context=Empty) exprs : [`Possible | `Unknown | `Impossible] =
    let tracker = get_tracker context in
    Tracker.setup tracker;
    match
      Z3.Solver.check (Tracker.state tracker) (List.map Term.zexpr exprs)
    with
    | Z3.Solver.SATISFIABLE   -> `Possible
    | Z3.Solver.UNSATISFIABLE -> `Impossible
    | Z3.Solver.UNKNOWN       -> `Unknown
end
