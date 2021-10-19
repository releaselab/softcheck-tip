type t = Uref | Uderef [@@deriving ord, sexp]

let pp ppf =
  let open Fmt in
  function Uref -> string ppf "&" | Uderef -> string ppf "*"

let to_string = Fmt.to_to_string pp
