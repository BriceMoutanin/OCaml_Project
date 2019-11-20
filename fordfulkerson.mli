open Graph

(* A path is a list of nodes. *)
type path = id list

(* find_path gr id1 id2 
 *    returns None if no path can be found.
 *    returns Some p if a path p from id1 to id2 has been found. 
 *)
val find_path: int graph -> id -> id -> path option

(* string_of_path p
 *    returns the path convert in string
 *)
val string_of_path: path option -> string

(* flow_min gr p
 *    returns the minimum flow possible found in the path p for the graph gr
 *
 * 	  raise Not_found if the path is not valid
 *)
val flow_min: int graph -> path -> int

(* actu_graph gr p actu
 *    returns a graph with the path p actualised with the value add
 *    (the edges in the same way of the path subtract the value actu to their label)
 *    (the edges in the opposite way of the path add the value actu to their label)
 *
 *    raise Not_found if the path is not valid
 *)
val actu_graph: int graph -> path -> int -> int graph

(* fordfulk gr source sink
 *    returns the graph gr on which FordFulkerson algorithm has been applied
 *
 *    source is the id of the source node
 *    sink is the id of the sink node
 *)
val fordfulk: int graph -> id -> id -> int graph