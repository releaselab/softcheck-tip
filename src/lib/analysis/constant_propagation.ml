(*open Batteries
  open Utils

  module Lattice = Lattices.Make_FlatLattice(struct type t = Ast.constant
    let to_string = string_of_int end)

  module L = Lattices.cre

  let rec eval env = let open Map.Infix in
  function
    Ast.Eident x        -> env --> x
  | Ast.Ecst x          -> Lattice.Flat x
  | Ast.Ebinop(o,e1,e2) ->
    let e_e1 = eval env e1 in
    let e_e2 = eval env e2 in
    begin match e_e1,e_e2 with
        Lattice.Flat x1, Lattice.Flat x2  -> begin
          match o with
            Ast.Beq   -> Lattice.Flat (if x1 = x2 then 1 else 0)
          | Ast.Bdiv  -> if x2 != 0 then Lattice.Flat (x1 / x2)
            else Lattice.Top
          | Ast.Bgt   -> Lattice.Flat (if x1 > x2 then 1 else 0)
          | Ast.Bsub  -> Lattice.Flat (x1 - x2)
          | Ast.Badd  -> Lattice.Flat (x1 + x2)
          | Ast.Bmul  -> Lattice.Flat (x1 * x2)
        end
      | Lattice.Bot, _ | _, Lattice.Bot -> Lattice.Bot
      | Lattice.Top, _ | _, Lattice.Top -> Lattice.Top
    end
  | Ast.Einput          -> Lattice.Top
  | Ast.Ecallf _ | Ast.Emalloc | Ast.Enull | Ast.Eunop _ -> assert false (* TODO *)

  let transfer s = let open Map.Infix in
  function
    Ast.Sassign (_,lv,rv) -> begin match lv with
        Ast.Eident x  -> s <-- (x,eval s rv)
      | _ -> s
    end
  | _ -> s

  let extremal _ = L.Some Set.empty
  let extremal_labels func = Set.singleton (Flow.init func.Ast.func_body)
  let flow = Flow.flow
  let f func (_,block) state = let state_value = match state with
      L.Bottom  -> bottom func
    | L.Some a  -> a
  in L.Some ((state_value -. kill func block) ||. gen block)
*)
