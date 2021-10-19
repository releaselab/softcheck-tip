open Scil_tip

include
  Softcheck_analysis.Very_busy_expressions.Make (Ast.Expr) (Cfg_node) (Cfg)
    (struct
      let aexp = Ast.aexp

      let contains_ident = Ast.contains_ident
    end)
