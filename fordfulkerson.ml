open Graph
open Tools

type path = id list

(* Un peu plus tard, trouver pourquoi cette fonction ne sert a rien*)
let create_residual_graph gr = e_fold gr (fun gr id1 id2 lb -> (add_arc gr id2 id1 0)) gr

let find_path gr id_l ident1 ident2 =
  let rec find_path_loop id_liste id1 id2 =

    (* Liste de tous les arcs sortants du noeud courant *)
    let liste_arcs = out_arcs gr id1 in

    (* On cherche un arc sortant valide*)
    let rec find_arc liste_arcs_sortants =
      match liste_arcs_sortants with
      (* S'il n'y a pas de noeuds suivants *)
      | [] -> None
      (* Si le noeud suivant a deja ete visite*)
      | (id,lb) :: tl when (List.exists (fun a -> a = id) id_liste) -> find_arc tl
      (* Si le noeud suivant implique un arc a 0*)                              
      | (id,lb) :: tl when lb = 0 -> find_arc tl
      (* Sinon *)
      | (id,lb) :: tl -> Some (id,lb)
    in

    (* On traite l'arc sortant *)
    match (find_arc liste_arcs) with
    (* S'il n'y avait pas d'arc sortant *)
    | None -> []
    (* Si l'arc sortant amene au noeud de destination *)
    | Some (id,lb) when id = id2 -> [id]
    (* Si l'arc sortant est valide mais le chemin n'est pas fini *)
    | Some (id,lb) -> id :: (find_path_loop (id::id_liste) id id2)
  in

  match (find_path_loop id_l ident1 ident2) with
  | [] -> None
  | l -> Some (ident1 :: l)


let rec string_of_path = function
  | None -> ""
  | Some [] -> ""
  | Some (a :: tl) -> (string_of_int a) ^ " -> " ^ (string_of_path (Some tl))

let flow_min gr p =
  let rec flow_min_loop min = function
    | None -> 0
    | Some [] -> 0
    | Some (id1 :: []) -> min
    | Some (id1 :: id2 :: tl) ->
      let new_min =
        match (find_arc gr id1 id2) with
        | None -> raise Not_found
        | Some x -> if x < min then x else min
      in
      flow_min_loop new_min (Some (id2::tl))
  in
  flow_min_loop max_int p
