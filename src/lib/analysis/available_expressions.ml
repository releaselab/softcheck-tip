open Scil_tip

include
  Softcheck_analysis.Available_expressions.Make (Ast.Expr) (Cfg_node) (Cfg)
    (struct
      let aexp = Ast.aexp

      let containst_lv = Ast.contains_ident
    end)
