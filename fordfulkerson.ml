open Graph
open Tools

type path = id list

(* Un peu plus tard, trouver pourquoi cette fonction ne sert a rien *)
let create_residual_graph gr = e_fold gr (fun gr id1 id2 lb -> (add_arc gr id2 id1 0)) gr

let find_path gr ident1 ident2 =
	Printf.printf "Recherche de chemin...\n" ;
  let rec find_path_loop id_liste id1 id2 =

    (* Liste de tous les arcs sortants du noeud courant *)
    let liste_arcs = out_arcs gr id1 in
    Printf.printf "Les noeuds suivant %i sont : " id1 ;
    let rec afficher_liste = function
    	| [] -> Printf.printf "\n"
    	| (id,lb) :: tl -> Printf.printf "%i (cout=%i) " id lb ;
    										 afficher_liste tl ;
    in
    afficher_liste liste_arcs ;

    (* On cherche un arc sortant valide *)
    let rec find_arc_valide liste_arcs_sortants =
      match liste_arcs_sortants with
      (* S'il n'y a pas de noeuds suivants *)
      | [] -> None
      (* Si le noeud suivant a deja ete visite *)
      | (id,lb) :: tl when (List.exists (fun a -> a = id) id_liste) -> Printf.printf "%i a deja ete visite\n" id ; find_arc_valide tl
      (* Si le noeud suivant implique un arc a 0 *)                              
      | (id,lb) :: tl when lb = 0 -> Printf.printf "%i ne peut pas etre atteint\n" id ; find_arc_valide tl
      (* Sinon *)
      | (id,lb) :: tl -> Printf.printf "On choisit %i\n" id ; Some (id,lb)
    in

    (* On traite l'arc sortant *)
    match (find_arc_valide liste_arcs) with
    (* S'il n'y avait pas d'arc sortant *)
    | None -> []
    (* Si l'arc sortant amene au noeud de destination *)
    | Some (id,lb) when id = id2 -> [id]
    (* Si l'arc sortant est valide mais le chemin n'est pas fini *)
    | Some (id,lb) -> id :: (find_path_loop (id::id_liste) id id2)
  in

  match (find_path_loop [] ident1 ident2) with
  | [] -> None
  | l when (List.exists (fun a -> a = ident2) l) -> Some (ident1 :: l)
  | l -> None


let rec string_of_path = function
  | None -> ""
  | Some [] -> ""
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
	match (find_path gr id1 id2) with
	| None -> gr
	| Some p -> fordfulk (actu_graph gr p (flow_min gr p)) id1 id2
	
	
