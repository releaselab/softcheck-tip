open Scil_tip

include
  Softcheck_analysis.Live_variables.Make (Ast.Expr) (Cfg_node) (Cfg)
    (struct
      let free_variables = Ast.free_variables
      let expr_of_var x = Ast.Expr.Eident x
    end)
