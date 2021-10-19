type t = Badd | Bsub | Bmul | Bdiv | Beq | Bgt [@@deriving ord, sexp]

val pp : Format.formatter -> t -> unit

val to_string : t -> string
