type t = Uref | Uderef [@@deriving ord, sexp]

val pp : Format.formatter -> t -> unit

val to_string : t -> string
