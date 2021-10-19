open Base

module T = struct
  module T_1 = struct
    type constant = int [@@deriving ord, sexp]

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
    [@@deriving ord, sexp]
  end

  include T_1
  include Comparable.Make (T_1)
end

include T

let rec pp ppf =
  let open Fmt in
  function
  | Ecst c -> int ppf c
  | Eident x -> string ppf x
  | Ebinop (o, a, b) -> pf ppf "(%a %a %a)" pp a Binop.pp o pp b
  | Eunop (o, e) -> pf ppf "(%a %a)" Unop.pp o pp e
  | Einput -> string ppf "input"
  | Ecallf (e, le) ->
      pf ppf "%s(%a)" e (list ~sep:(fun ppf () -> pf ppf ", ") pp) le
  | Ecallfptr (e, le) ->
      pf ppf "%a(%a)" pp e (list ~sep:(fun ppf () -> pf ppf ", ") pp) le
  | Emalloc -> string ppf "malloc"
  | Enull -> string ppf "null"

let to_string = Fmt.to_to_string pp
