include
  Softcheck.Cfg.Node_specifics.Make (Ast.Expr) (Cfg_node)
    (struct
      type expr = Ast.Expr.t

      let free_variables_expr = Ast.free_variables
    end)
