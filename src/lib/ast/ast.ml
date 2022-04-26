open Base
module Binop = Binop
module Unop = Unop
module Expr = Expr
module Stmt = Stmt

type idents = string list

type func = {
  func_id : string;
  func_args : idents;
  func_vars : idents;
  func_body : Stmt.t;
  func_return : Expr.t;
}

type program = func list

let aexp =
  let open Expr in
  let rec aexpr_rec acc = function
    | Ebinop (op, e1, e2) as e -> (
        match op with
        | Beq | Bgt -> aexpr_rec (aexpr_rec acc e1) e2
        | Badd | Bdiv | Bmul | Bsub -> Set.add acc e)
    | Ecallf _ | Ecallfptr _ | Eident _ | Eunop _ | Ecst _ | Enull | Einput
    | Emalloc ->
        acc
  in
  aexpr_rec (Set.empty (module Expr))

let free_variables =
  let rec free_variables_rec acc =
    let open Expr in
    function
    | Ecallf (_, params) | Ecallfptr (_, params) ->
        List.fold_left ~f:free_variables_rec ~init:acc params
    | Eident x -> Set.add acc x
    | Ebinop (_, e1, e2) -> free_variables_rec (free_variables_rec acc e1) e2
    | Eunop (_, e) -> free_variables_rec acc e
    | Einput | Emalloc | Enull | Ecst _ -> acc
  in
  free_variables_rec (Set.empty (module String))

let is_ident = function Expr.Eident _ -> true | _ -> false

let ident_of_expr = function
  | Expr.Eident x -> x
  | _ -> raise (Invalid_argument "not and ident")

let uses_lv_expr x =
  let rec uses_lv_expr =
    let open Expr in
    function
    | Ebinop (_, e1, e2) -> uses_lv_expr e1 || uses_lv_expr e2
    | Ecallf (_, vars) | Ecallfptr (_, vars) -> List.exists ~f:uses_lv_expr vars
    | Eident _ as i -> i = x
    | Eunop (_, e) -> uses_lv_expr e
    | Ecst _ | Enull | Einput | Emalloc -> false
  in
  uses_lv_expr

let uses_var x =
  let rec uses_var = function
    | Expr.Ebinop (_, e1, e2) -> uses_var e1 || uses_var e2
    | Ecallf (_, vars) | Ecallfptr (_, vars) -> List.exists ~f:uses_var vars
    | Eident v -> String.(v = x)
    | Eunop (_, e) -> uses_var e
    | Ecst _ | Enull | Einput | Emalloc -> false
  in
  uses_var
