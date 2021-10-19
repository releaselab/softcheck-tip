open Base

include
  Softcheck_analysis.Sign_analysis.Make (Ast.Expr) (Scil_tip.Cfg_node)
    (Scil_tip.Cfg)
    (struct
      let is_ident =
        let open Ast.Expr in
        function Eident _ -> true | _ -> false

      let ident_of_expr =
        let open Ast.Expr in
        function Eident x -> x | _ -> assert false

      let expr_sign_eval env =
        let rec aux =
          let open Ast.Expr in
          function
          | Eident x -> env x
          | Ecst x -> Lattices.Sign.sign x
          | Ebinop (op, e1, e2) -> (
              match op with
              | Badd -> Lattices.Sign.plus (aux e1) (aux e2)
              | Bsub -> Lattices.Sign.minus (aux e1) (aux e2)
              | Bmul -> Lattices.Sign.times (aux e1) (aux e2)
              | Bdiv -> Lattices.Sign.divide (aux e1) (aux e2)
              | Beq | Bgt -> raise (Invalid_argument "cannot eval sign"))
          | Einput -> Lattices.Sign.top
          | Ecallf _ | Ecallfptr _ | Eunop _ | Emalloc | Enull ->
              raise (Invalid_argument "cannot eval sign")
        in
        aux
    end)

(*module ContextSensitiveSignAnalysis(P : sig val p : Ast.program end) = struct
  module Intra = Make(P)

  module L = Lattices.PowersetLattice(struct
      type t = Intra.L.property * Intra.L.property
      let to_string (c,l) = Printf.sprintf "%s -> %s" (Intra.L.to_string c) (Intra.L.to_string l)
    end)

  let graph = InterCfg.generate_from_program P.p
  let blocks = InterCfg.get_blocks graph

  let eval_args f formal actual s =
    List.fold_left2 (fun acc id exp -> Intra.L.set acc (id,f) (Intra.eval s f exp))
      Intra.L.bottom formal actual

  module F = struct
    type label = Cfg.stmt_label
    type vertex = Cfg.vertex
    type state = L.property
    type ident = Cfg.ident

    let f1 _ l b s = let open InterFlow in
      match b with
        Scallassign (_, rv) -> begin match rv with
          Ast.Ecallf (f_id, actual) ->
            let formal =
              InterCfg.outflow graph l |> List.find_map
                (fun l' -> InterCfg.get graph l' |>
                           function Sbegin args -> Some args
                                  | Scallassign _ | Swhile _ | Send
                                  | Saftercallassign _ | Sassign _ | Sif _
                                  | Soutput _ | Sreturn _ -> None) in
            Set.map (fun (_,d) -> let d' = eval_args f_id formal actual d in
                      d,d') s
        | Ast.Ecallfptr _ -> assert false (* TODO *)
        | Ast.Ebinop _ | Ast.Ecst _ | Ast.Eident _ | Ast.Einput | Ast.Emalloc
        | Ast.Enull | Ast.Eunop _ -> assert false
      end
      | Send | Sbegin _ | Saftercallassign _ | Sassign _ | Sif _ | Soutput _
      | Swhile _ | Sreturn _ -> assert false

    let f2 fid fid' _ b s1 s2 = let open InterFlow in
      match b with
        Saftercallassign (lv, _) -> Set.map (fun (c1,s1') ->
        let c2 = s1' in
        let s2' = Set.find_prep ((=) c2 % fst) s2 |> snd in
        c1, Intra.eval s2' fid' (Ast.Eident "return") |> Intra.L.set s1' (lv,fid)) s1
      | Scallassign _ | Sbegin _ | Send | Sassign _ | Sif _
      | Soutput _ | Swhile _ | Sreturn _ -> assert false

    let f fid _ b s = let open InterFlow in
      match b with
        Sassign (lv,rv) -> begin let open Ast in match lv with
        Eident id ->
          Set.map (fun (c,s') -> c, Intra.eval s' fid rv |> Intra.L.set s' (id,fid)) s
      | Ebinop _ | Ecallf _ | Ecallfptr _ | Eunop _ | Ecst _ | Einput
      | Emalloc | Enull -> s
      end
      | Sreturn e ->
          Set.map (fun (c,s') -> c, Intra.eval s' fid e |> Intra.L.set s' ("return",fid)) s
      | Sbegin _ | Saftercallassign _ | Sif _ | Soutput _
      | Swhile _ -> s
      | Scallassign _ | Send -> assert false

    let initial_state =
      let i = Set.fold (fun x map -> Map.add x Sign_lattice.Top map) Intra.declaredVars Map.empty in
      Set.singleton (i, i)
  end
  end
*)
