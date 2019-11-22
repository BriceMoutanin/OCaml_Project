open Adoption

type path = string

val from_bfile: path -> (adopteur list * adopte list)
