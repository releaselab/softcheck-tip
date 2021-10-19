type t =
  | Sassign of Expr.t * Expr.t
  | Sblock of t list
  | Sif of Expr.t * t
  | Sifelse of Expr.t * t * t
  | Soutput of Expr.t
  | Swhile of Expr.t * t

and block = t list

val pp : Format.formatter -> t -> unit

val to_string : t -> string
