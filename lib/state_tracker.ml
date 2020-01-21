module Make (P : sig
    type state
    type operation

    val push : state -> unit
    val pop : state -> int -> unit
    val apply : state -> operation list -> unit
  end) :
sig
  type t

  val make : P.state -> t
  val apply : P.operation -> t -> t

  val state : t -> P.state
  val fold_operation : t -> (P.operation -> 'a -> 'a) -> 'a -> 'a

  val loose_setup : t -> P.operation list
  val setup : t -> unit
end =
struct

  type cell =
    | Null
    | Cell of { depth: int; mutable saved: bool; op: P.operation; next: cell }

  type state = {
    p : P.state;
    mutable cursor : cell;
  }

  type t = { state: state; cell: cell }

  let make p = {state = {p; cursor = Null}; cell = Null}
  let state t = t.state.p

  let ancestor c1 c2 = match c1, c2 with
    | Null, _ | _, Null -> Null
    | Cell x1, Cell x2 ->
      let rec go_up c1 c2 =
        if c1 == c2 then c1
        else match c1, c2 with
          | Cell c1', Cell c2' ->
            assert (c1'.depth = c2'.depth);
            go_up c1'.next c2'.next
          | _ -> assert false
      in
      let rec seek n = function
        | Null -> assert (n = 0); Null
        | Cell c as cell ->
          if c.depth = n then cell
          else (assert (c.depth > n); seek n c.next)
      in
      if x1.depth > x2.depth
      then go_up (seek x2.depth c1) c2
      else go_up c1 (seek x1.depth c2)

  let depth = function Null -> 0 | Cell x -> x.depth

  let apply op t =
    let next = t.cell in
    let cell = Cell { depth = depth next + 1; saved = false; op; next } in
    { state = t.state; cell }

  let push state cell todo =
    match cell, todo with
    | _, [] -> ()
    | Null, _ -> assert false
    | Cell x, _ ->
      P.push state.p;
      P.apply state.p todo;
      state.cursor <- cell;
      x.saved <- true

  let loose_setup {state; cell} =
    let ancestor = ancestor state.cursor cell in
    let rec rewind acc = function
      | Null -> acc
      | xs when xs == ancestor -> acc
      | Cell x ->
        let acc = if x.saved then (x.saved <- false; acc + 1) else acc in
        rewind acc x.next
    in
    P.pop state.p (rewind 0 state.cursor);
    state.cursor <- ancestor;
    let snapshot =
      let rec snapshot = function
        | Null -> Null
        | Cell {saved = true; _} as cell -> cell
        | Cell x -> snapshot x.next
      in
      snapshot ancestor
    in
    (* At this point:
       - cell is the state we would like to get to
       - snapshot is a tail of cell and the current global state
    *)
    let snapshot_depth =
      let dsnapshot = depth snapshot in
      let dcells = depth cell in
      dsnapshot + (dcells - dsnapshot + 1) / 2
    in
    let rec replay = function
      | Null -> [] | xs when xs == snapshot -> []
      | Cell x as cell ->
        assert (not x.saved);
        let todo = x.op :: replay x.next in
        if x.depth = snapshot_depth then (
          push state cell (List.rev todo);
          []
        ) else
          todo
    in
    List.rev (replay cell)

  let setup t =
    let ops = loose_setup t in
    push t.state t.cell ops

  let fold_operation t f acc =
    let rec aux acc = function
      | Null -> acc
      | Cell c -> aux (f c.op acc) c.next
    in
    aux acc t.cell
end
