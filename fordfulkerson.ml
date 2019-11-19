open Graph
open Tools

type path = id list

(* Un peu plus tard, trouver pourquoi cette fonction ne sert a rien *)
let create_residual_graph gr = e_fold gr (fun gr id1 id2 lb -> (add_arc gr id2 id1 0)) gr

let rec find_path gr id1 id2 =

	let arcs_sortants = out_arcs gr id1 in
	
	let rec find_path_loop id_c forbidden l_arcs_sortants =
	
		(* Liste de tous les arcs sortants du noeud courant *)
   	Printf.printf "\n\n\nLes noeuds suivants %i qui restent sont : " id_c ;
   	let rec afficher_liste = function
    	| [] -> Printf.printf "\n" ;
    	| (id,lb) :: tl -> Printf.printf "%i (cout=%i) " id lb ;
    					   afficher_liste tl ;
   	in
   	afficher_liste l_arcs_sortants ;
	
		(* Affichage de la pile *)
 	 	Printf.printf "Pile : " ;
  	let rec afficher_pile = function
    	| [] -> Printf.printf "\n" ;
    	| id :: tl -> Printf.printf "%i ; " id ;
    						  	afficher_pile tl ;
    in
    afficher_pile forbidden ;
	 
	 (* Parcours de tous les noeuds suivants *)
	 match l_arcs_sortants with
	 	(* S'il n'y a pas de noeuds suivants *)
		| [] -> []
		(* Si le noeud suivant a deja ete visite *)
		| (id,lb)::tl when (List.exists (fun a -> a = id) forbidden) -> Printf.printf "%i a deja ete visite\n" id ; find_path_loop id_c forbidden tl
		(* Si le noeud suivant est inatteignable (flot=0) *)
		| (id,lb)::tl when lb=0 -> Printf.printf "%i est inatteignable\n" id ; find_path_loop id_c forbidden tl
		(* Si on est arrive au dernier noeud *)
		| (id,lb)::tl when id=id2 -> [id2]
		(* S'il existe un autre noeud suivant plus loin qui offre un arc avec un meilleur flot *)
		| (id,lb)::tl when (List.exists (fun (a,b) -> (b > lb) && not (find_path_loop a (id_c::forbidden) (out_arcs gr a)=[])) tl) -> find_path_loop id_c forbidden tl
		(* Si le noeud semble tre ok *)
		| (id,lb)::tl -> Printf.printf "On choisit %i\n" id ; (* On vŽrifie quel chemin propose ce noeud *)
																													begin match (find_path_loop id (id_c::forbidden) (out_arcs gr id)) with
																														(* Si le chemin n'aboutit pas, on passe au noeud suivant *)
																														| [] -> find_path_loop id_c (List.append (id_c::forbidden) [id]) tl
																														(* Si le chemin aboutit *)
																														| l -> id::l
																													end
	
	in
	
	let resultat = find_path_loop id1 [] arcs_sortants in
	if resultat = [] then None else Some (id1::resultat)

let rec string_of_path = function
  | None -> ""
  | Some [] -> ""
  | Some (a :: []) -> (string_of_int a)
  | Some (a :: tl) -> (string_of_int a) ^ " -> " ^ (string_of_path (Some tl))

let flow_min gr p =
	Printf.printf "Recherche du min dans %s\n" (string_of_path (Some p)) ;
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
  
let actu_graph gr p add =
	Printf.printf "Actualisation du chemin avec un min de %i\n" add ;
	let rec actu_loop gr p add =
	match p with
	| id1::id2::[] -> let new_gr = add_arc gr id1 id2 (add*(-1)) in
										add_arc new_gr id2 id1 add
	| id1::id2::tl -> let new_gr = add_arc gr id1 id2 (add*(-1)) in
										let new_gr = add_arc new_gr id2 id1 add in
										actu_loop new_gr (id2::tl) add
	| _ -> raise Not_found
	in
	actu_loop gr p add
	
let rec fordfulk gr id1 id2 =
	Printf.printf "Test\n" ;
	match (find_path gr id1 id2) with
	| None -> gr
	| Some p -> fordfulk (actu_graph gr p (flow_min gr p)) id1 id2