open Scil_tip

include
  Softcheck_analysis.Available_expressions.Make (Ast.Expr) (Cfg_node) (Cfg)
    (struct
      let aexp = Ast.aexp
      let free_variables = Ast.free_variables
      let uses_var = Ast.uses_var
      let uses_lv_expr = Ast.uses_lv_expr
    end)
