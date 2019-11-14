open Graph

(* A path is a list of nodes. *)
type path = id list

val create_residual_graph: int graph -> int graph

(* find_path gr forbidden id1 id2 
 *   returns None if no path can be found.
 *   returns Some p if a path p from id1 to id2 has been found. 
 *
 *  forbidden is a list of forbidden nodes (they have already been visited)
 *)
val find_path: int graph -> id list -> id -> id -> path option

val string_of_path: path option -> string

val flow_min: int graph -> path option -> int
