open Graph
open Tools

type path = id list

(* Un peu plus tard, trouver pourquoi cette fonction ne sert a rien *)
let create_residual_graph gr = e_fold gr (fun gr id1 id2 lb -> (add_arc gr id2 id1 0)) gr

let find_path gr ident1 ident2 =
	Printf.printf "Recherche de chemin...\n" ;
  let rec find_path_loop id_liste id1 =

    (* Liste de tous les arcs sortants du noeud courant *)
    let liste_arcs = out_arcs gr id1 in
    
    Printf.printf "Liste des noeuds interdits : " ;
    let rec afficher_pile = function
      | [] -> Printf.printf "\n" ;
      | id :: tl -> Printf.printf "%i " id ; afficher_pile tl ;
    in
    afficher_pile id_liste ;
    
    Printf.printf "Les noeuds suivant %i sont : " id1 ;
    let rec afficher_liste = function
      | [] -> Printf.printf "\n" ;
      | (id,lb) :: tl -> Printf.printf "%i (cout=%i) " id lb ; afficher_liste tl ;
    in
    afficher_liste liste_arcs ;

    let rec parcours_liste = function
      | [] -> None
      | (id,lb) :: tl when lb=0 -> parcours_liste tl
      | (id,lb) :: tl when (List.exists (fun a -> a = id) id_liste) -> parcours_liste tl
      | (id,lb) :: tl ->
        match (find_path_loop (id1::id_liste) id) with
        | None -> parcours_liste tl
        | Some p -> Some (id1::p)
    in
    parcours_liste liste_arcs
  in
  find_path_loop [ident1] ident1

    (*(* On cherche un arc sortant valide *)
    let rec find_next_node liste_arcs_sortants =
      match liste_arcs_sortants with
      (* S'il n'y a pas de noeuds suivants *)
      | [] -> None
      (* Si le noeud suivant a deja ete visite *)
      | (id,lb) :: tl when (List.exists (fun a -> a = id) id_liste) -> Printf.printf "%i a deja ete visite\n" id ; find_next_node tl
      (* Si le noeud suivant implique un arc a 0 *)                              
      | (id,lb) :: tl when lb = 0 -> Printf.printf "%i ne peut pas etre atteint\n" id ; find_next_node tl
      (* Sinon *)
      | (id,lb) :: tl -> Printf.printf "On choisit %i\n" id ; Some id
    in

    (* On traite l'arc sortant *)
    match (find_next_node liste_arcs) with
    (* S'il n'y avait pas d'arc sortant *)
    | None ->
      begin match id_liste with
        | pere :: tl -> find_path_loop (id1::tl) pere
        | [] -> []
      end
    (* Si l'arc sortant amene au noeud de destination *)
    | Some id when id = ident2 -> [id]
    (* Si l'arc sortant est valide mais le chemin n'est pas fini *)
    | Some id -> id :: (find_path_loop (id1::id_liste) id )
  in

  match (find_path_loop [] ident1) with
  | [] -> None
      | l -> Some (ident1 :: l)*)


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
  Printf.printf "Actualisation du chemin avec un min de %i\n" add;
  let rec actu_loop gr p add =
    match p with
    | id1::id2::[] -> let new_gr = add_arc gr id1 id2 (add*(-1))
      in
      add_arc new_gr id2 id1 add
    | id1::id2::tl -> let new_gr = add_arc gr id1 id2 (add*(-1))
      in
      let new_gr = add_arc new_gr id2 id1 add
      in
      actu_loop new_gr (id2::tl) add
    | _ -> raise Not_found
  in
  actu_loop gr p add
	
let rec fordfulk gr id1 id2 =
  match (find_path gr id1 id2) with
  | None -> gr
  | Some p -> fordfulk (actu_graph gr p (flow_min gr p)) id1 id2
	
	
