open Adoption

type path = string

val read_adopteur: int -> adopteur list -> string -> adopteur list

val read_adopte: int -> adopte list -> string -> adopte list

val from_file: path -> (adopteur list * adopte list)
