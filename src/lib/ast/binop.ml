type t = Badd | Bsub | Bmul | Bdiv | Beq | Bgt [@@deriving ord, sexp]

let pp ppf x =
  let open Fmt in
  let s =
    match x with
    | Badd -> "+"
    | Bsub -> "-"
    | Bmul -> "*"
    | Bdiv -> "/"
    | Beq -> "=="
    | Bgt -> ">"
  in
  string ppf s

let to_string = Fmt.to_to_string pp
