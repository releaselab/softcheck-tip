open Base

type constant = int

type t =
  | Ecallf of string * t list
  | Ecallfptr of t * t list
  | Eident of string
  | Ebinop of Binop.t * t * t
  | Eunop of Unop.t * t
  | Ecst of constant
  | Einput
  | Emalloc
  | Enull

include Comparable.S with type t := t

include Sexpable.S with type t := t

val pp : Formatter.t -> t -> unit

val to_string : t -> string
