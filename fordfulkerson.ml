open Graph
open Tools

type path = id list

(* find_path gr id1 id2 
 *    returns None if no path can be found.
 *    returns Some p if a path p from id1 to id2 has been found. 
 *)
let rec find_path gr id1 id2 =
	let rec find_path_loop id_c forbidden l_arcs_sortants =
	 (* Parcours de tous les noeuds suivants *)
	 match l_arcs_sortants with
	 	(* S'il n'y a pas de noeuds suivants *)
		| [] -> []
		(* Si le noeud suivant a deja ete visite *)
		| (id,lb)::tl when (List.exists (fun a -> a = id) forbidden) -> find_path_loop id_c forbidden tl
		(* Si le noeud suivant est inatteignable (flot=0) *)
		| (id,lb)::tl when lb=0 -> find_path_loop id_c forbidden tl
		(* Si on est arrive au dernier noeud *)
		| (id,lb)::tl when id=id2 -> [id2]
		(* S'il existe un noeud suivant plus loin dans la liste qui offre un arc avec un meilleur flot *)
		| (id,lb)::tl when (List.exists (fun (a,b) -> (b > lb) && not (find_path_loop a (id_c::forbidden) (out_arcs gr a)=[])) tl) -> find_path_loop id_c forbidden tl
		(* Si le noeud suivant semble tre ok *)
		| (id,lb)::tl ->(* On vŽrifie quel chemin propose ce noeud *)
										begin match (find_path_loop id (id_c::forbidden) (out_arcs gr id)) with
									  	(* Si le chemin n'aboutit pas, on passe au noeud suivant *)
											| [] -> find_path_loop id_c (List.append (id_c::forbidden) [id]) tl
											(* Si le chemin aboutit *)
											| l -> id::l
										end
	in
	let resultat = find_path_loop id1 [] (out_arcs gr id1) in
	if resultat = [] then None else Some (id1::resultat)

(* string_of_path p
 *    returns the path convert in string
 *)
let rec string_of_path = function
  | None -> ""
  | Some [] -> ""
  | Some (a :: []) -> (string_of_int a)
  | Some (a :: tl) -> (string_of_int a) ^ " -> " ^ (string_of_path (Some tl))

(* flow_min gr p
 *    returns the minimum flow found in the path p for the graph gr
 *
 * 	  raise Not_found if the path is not valid
 *)
let flow_min gr p =
  let rec flow_min_loop min = function
    | [] -> min
    | id1 :: [] -> min
    | id1 :: id2 :: tl ->
      let new_min =
        match (find_arc gr id1 id2) with
        | None -> raise Not_found
        | Some x -> if x < min then x else min
      in
      flow_min_loop new_min (id2::tl)
  in
  flow_min_loop max_int p

(* actu_graph gr p actu
 *    returns a graph with the path p actualised with the value add
 *    (the edges in the same way of the path subtract the value actu to their label)
 *    (the edges in the opposite way of the path add the value actu to their label)
 *
 *    raise Not_found if the path is not valid
 *)
let actu_graph gr p actu =
	let rec actu_loop gr p actu =
	match p with
	| id1::id2::[] -> let new_gr = add_arc gr id1 id2 (actu*(-1)) in
										add_arc new_gr id2 id1 actu
	| id1::id2::tl -> let new_gr = add_arc gr id1 id2 (actu*(-1)) in
										let new_gr = add_arc new_gr id2 id1 actu in
										actu_loop new_gr (id2::tl) actu
	| _ -> raise Not_found
	in
	actu_loop gr p actu

(* fordfulk gr source sink
 *    returns the graph gr which FordFulkerson algorithm has been applied
 *
 *    source is the id of the source node
 *    sink is the id of the sink node
 *)
let rec fordfulk gr source sink =
	match (find_path gr source sink) with
	| None -> gr
	| Some p -> fordfulk (actu_graph gr p (flow_min gr p)) source sink