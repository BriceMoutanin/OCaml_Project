type path = Graph.id list

type arc_flow = { flow : int ; capacity : int }

let find_path gr id_l ident1 ident2 =
  let rec find_path_loop id_liste id1 id2 =

    (* Liste de tous les arcs sortants du noeud courant *)
    let liste_arcs = Graph.out_arcs gr id1 in

    (* On cherche un arc sortant valide*)
    let rec find_arc liste_arcs_sortants =
      match liste_arcs_sortants with
      (* S'il n'y a pas de noeuds suivants *)
      | [] -> None
      (* Si le noeud suivant a deja ete visite*)
      | (id,{flow = f ; capacity = c}) :: tl when (List.exists (fun a -> a = id) id_liste) -> find_arc tl
      (* Si l'arc considere est plein (flow = capacite) *)
      | (id,{flow = f ; capacity = c}) :: tl when f = c -> find_arc tl
      (* Sinon *)
      | (id,lb) :: tl -> Some (id,lb)
    in

    (* On traite l'arc sortant *)
    match (find_arc liste_arcs) with
    (* S'il n'y avait pas d'arc sortant *)
    | None -> []
    (* Si l'arc sortant amene au noeud de destination *)
    |	Some (id,lb) when id = id2 -> [id]
    (* Si l'arc sortant est valide mais le chemin n'est pas fini *)
    | Some (id,lb) -> id :: (find_path_loop (id::id_liste) id id2)
  in

  match (find_path_loop id_l ident1 ident2) with
  | [] -> None
  | l -> Some (ident1 :: l)


let rec string_of_path = function
  | None -> ""
  | Some [] -> ""
  | Some (a :: tl) -> (string_of_int a) ^ " " ^ (string_of_path (Some tl))

