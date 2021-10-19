open Base
open Scil_tip

module S = struct
  type expr = Ast.Expr.t

  type ident = string [@@deriving eq, ord]

  type vertex = Cfg.Vertex.t [@@deriving eq, ord]

  type blocks = Set.M(Cfg.Vertex).t

  type definition_location =
    Softcheck_analysis.Reaching_definitions.Definition_location.t

  let free_variables = Ast.free_variables

  let is_ident = function Ast.Expr.Eident _ -> true | _ -> false

  let ident_of_expr = function Ast.Expr.Eident x -> x | _ -> assert false
end

include
  Softcheck_analysis.Reaching_definitions.Make (Ast.Expr) (Cfg_node) (Cfg) (S)
