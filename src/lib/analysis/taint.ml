include
  Softcheck_analysis.Taint.Make (Ast.Expr) (Scil_tip.Cfg_node) (Scil_tip.Cfg)
    (struct
      include Reaching_definitions.S

      let rec eval s =
        let open Ast.Expr in
        let open Lattices.Taint in
        function
        | Ecallf _ | Ecallfptr _ -> bottom
        | Eident i -> s i
        | Eunop (_, e) -> eval s e
        | Ecst _ -> Element false
        | Einput -> Element true
        | Emalloc -> Element false
        | Enull -> Element false
        | Ebinop (_, e1, e2) -> (
            let eval_e1 = eval s e1 in
            let eval_e2 = eval s e2 in
            match (eval_e1, eval_e2) with
            | Top, _ | _, Top -> Top
            | Element true, _ | _, Element true -> Element true
            | Bottom, _ | _, Bottom -> Bottom
            | _ -> Element false)
    end)
