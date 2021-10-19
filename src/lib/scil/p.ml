type t = Ast.program

let stmt_to_node next_label =
  let open Ast.Stmt in
  let open Ast.Expr in
  let open Scil_impl.Stmt in
  let rec aux s =
    let stmt_label = next_label () in
    let stmt_s =
      match s with
      | Sassign (lv, rv) -> Scil_assign (lv, rv)
      | Soutput e ->
          let f = Eident "output" in
          Scil_call (f, [ e ])
      | Sif (e, b) -> Scil_if (e, aux b)
      | Sifelse (e, ib, eb) -> Scil_if_else (e, aux ib, aux eb)
      | Swhile (e, b) -> Scil_while (e, aux b)
      | Sblock (h :: t) ->
          let s =
            List.fold_left
              (fun acc s ->
                let stmt_label = -1 in
                let stmt_s = Scil_seq (acc, aux s) in
                { stmt_s; stmt_label })
              (aux h) t
          in
          s.stmt_s
      | Sblock [] -> assert false
    in
    { stmt_s; stmt_label }
  in
  aux

let global_decls _ _ = [] (* no global declarations in TIP *)

let funcs next_label =
  let open Scil_impl.Stmt in
  let process_decls = function
    | [] -> assert false
    | h :: t ->
        let init =
          let stmt_s = Scil_var_decl h in
          { stmt_label = next_label (); stmt_s }
        in
        List.fold_left
          (fun acc d ->
            let stmt_label = -1 in
            let stmt_s =
              let stmt_label = next_label () in
              let stmt_s = Scil_var_decl d in
              Scil_seq (acc, { stmt_label; stmt_s })
            in
            { stmt_s; stmt_label })
          init t
  in
  List.map (fun f ->
      let decls = process_decls f.Ast.func_vars in
      ( f.Ast.func_id,
        f.Ast.func_vars,
        {
          stmt_label = -1;
          stmt_s = Scil_seq (decls, stmt_to_node next_label f.Ast.func_body);
        } ))

let convert_program_to_sl : t -> Scil_impl.program =
 fun p ->
  let label = ref (-1) in
  let next_label () =
    let () = label := !label + 1 in
    !label
  in
  (global_decls next_label p, funcs next_label p)
