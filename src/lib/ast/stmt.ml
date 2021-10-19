type t =
  | Sassign of Expr.t * Expr.t
  | Sblock of t list
  | Sif of Expr.t * t
  | Sifelse of Expr.t * t * t
  | Soutput of Expr.t
  | Swhile of Expr.t * t

and block = t list

let rec pp ppf =
  let open Fmt in
  function
  | Sassign (lv, rv) -> pf ppf "%a = %a;" Expr.pp lv Expr.pp rv
  | Sblock b -> pf ppf "{%a}" (list ~sep:(fun ppf () -> string ppf "\n") pp) b
  | Sif (e, ifb) -> pf ppf "if (%a) %a" Expr.pp e pp ifb
  | Sifelse (e, ifb, elseb) ->
      pf ppf "if (%a) %a\nelse%a" Expr.pp e pp ifb pp elseb
  | Soutput e -> pf ppf "output %a;" Expr.pp e
  | Swhile (e, whileb) -> pf ppf "while (%a) %a" Expr.pp e pp whileb

let to_string = Fmt.to_to_string pp
