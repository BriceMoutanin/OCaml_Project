open Graph
open Adoption

(* Renvoie vrai s'il existe au moins un ŽlŽment commun entre deux listes *)
let rec commun l1 l2 =
	match l1 with
		| [] -> false
		| a :: tl when List.exists (fun x -> a=x) l2 -> true
		| a :: tl -> commun tl l2
		
let rec nb_participants l_adopteurs l_adoptes =
	match (l_adopteurs,l_adoptes) with
	| ([],[]) -> 0
	| (a::tl1,b::tl2) -> 2 + (nb_participants tl1 tl2)
	| (a::tl,[]) -> 1 + (nb_participants tl [])
	| ([],a::tl) -> 1 + (nb_participants [] tl)

let rec construire_paires l_adopteurs l_adoptes =
	match l_adopteurs with
	| [] -> []
	| adopteur :: tl -> let rec trouver_adopte l =
												match l with
													| [] -> []
													| adopte :: tl2 when adopte.taille < adopteur.taille_min -> trouver_adopte tl2
													| adopte :: tl2 when not (List.exists (fun x -> x=adopte.cheveux) adopteur.cheveux_rech) -> trouver_adopte tl2
													| adopte :: tl2 when not (commun adopteur.relation_recherchee_c adopte.relation_recherchee_a) -> trouver_adopte tl2
													| adopte :: tl2 when not (commun adopteur.mes_loisirs_c adopte.mes_loisirs_a) -> trouver_adopte tl2
													| adopte :: tl2 -> (adopteur.id_c , adopte.id_a) :: trouver_adopte tl2
											in
											List.append (trouver_adopte l_adoptes) (construire_paires tl l_adoptes)
											
let gr_biparti l_adopteurs l_adoptes gr =
	let nb_part = nb_participants l_adopteurs l_adoptes in
	let id_source = 0 in
	let id_puits = nb_part + 1 in
	let id_l = construire_paires l_adopteurs l_adoptes in
	let ngr = new_node gr id_source in
	let ngr = new_node ngr id_puits in
	let rec construire_gr_biparti l gr =
	match l with
	| [] -> gr
	| (id_adopteur,id_adopte) :: tl when (node_exists gr id_adopteur) && (node_exists gr id_adopte) -> let gr1 = new_arc gr id_adopteur id_adopte 1 in
																																																		 construire_gr_biparti tl gr1
	| (id_adopteur,id_adopte) :: tl when not (node_exists gr id_adopteur) && not (node_exists gr id_adopte) -> let gr1 = new_node gr id_adopteur in
																																																						 let gr1 = new_node gr1 id_adopte in
																																																						 let gr1 = new_arc gr1 id_source id_adopteur 1 in
																																																						 let gr1 = new_arc gr1 id_adopte id_puits 1 in
																																																						 let gr1 = new_arc gr1 id_adopteur id_adopte 1 in
																																																						 construire_gr_biparti tl gr1
	| (id_adopteur,id_adopte) :: tl when not (node_exists gr id_adopteur) -> let gr1 = new_node gr id_adopteur in
																																					 let gr1 = new_arc gr1 id_source id_adopteur 1 in
																																					 let gr1 = new_arc gr1 id_adopteur id_adopte 1 in
											                                										 construire_gr_biparti tl gr1
	| (id_adopteur,id_adopte) :: tl -> let gr1 = new_node gr id_adopte in
																		 let gr1 = new_arc gr1 id_adopte id_puits 1 in
												             let gr1 = new_arc gr1 id_adopteur id_adopte 1 in
											               construire_gr_biparti tl gr1
	in
	let ngr = construire_gr_biparti id_l ngr in
	let rec completer_noeuds gr = function
		| 0 -> gr
		| n when (node_exists gr n) -> completer_noeuds gr (n-1)
			  | n -> completer_noeuds (new_node gr n) (n-1)
	in
  completer_noeuds ngr nb_part