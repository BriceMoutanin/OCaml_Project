open Graph
open Adoption

val nb_participants : adopteur list -> adopte list -> int

val construire_paires : adopteur list -> adopte list -> (id * id) list

val gr_biparti : adopteur list -> adopte list -> int graph -> int graph