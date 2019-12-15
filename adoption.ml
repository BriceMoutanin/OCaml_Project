(* Le module Adoption permet de créer et gérer les structures de données
 * qui vont contenir les différentes profils pour l'application de matching
 *)

type couleur_cheveux = Brun | Blond | Roux
type relation = Serieuse | Aventure | Amicale
type loisirs = Sport | Lecture | Sorties | Musique | Films | Gastronomie | JeuxVideo | ArtsPlastiques

type adopteur = {
	id_c: int;
	nom_c: string;
	cheveux_rech: couleur_cheveux list;
	taille_min: float;
	relation_recherchee_c: relation list;
	mes_loisirs_c: loisirs list;
}

type adopte = {
	id_a: int;
	nom_a: string;
	cheveux: couleur_cheveux;
	taille: float;
	relation_recherchee_a: relation list;
	mes_loisirs_a: loisirs list;	
}

(* construire_liste_cheveux l
 * l est une liste de string qui correspondent à une couleur de cheveux
 * renvoie une liste de couleur_cheveux
 *)
let rec construire_liste_cheveux = function
	| [] -> []
	| "brun" :: tl -> Brun :: (construire_liste_cheveux tl)
	| "blond" :: tl -> Blond :: (construire_liste_cheveux tl)
	| "roux" :: tl -> Roux :: (construire_liste_cheveux tl)
	| _ :: tl -> raise Not_found

(* construire_liste_rel l
 * l est une liste de string qui correspondent à un type de relation
 * renvoie une liste de relation
 *)
let rec construire_liste_rel = function
	| [] -> []
	| "serieuse" :: tl -> Serieuse :: (construire_liste_rel tl)
	| "amicale" :: tl -> Amicale :: (construire_liste_rel tl)
	| "aventure" :: tl -> Aventure :: (construire_liste_rel tl)
	| _ :: tl -> raise Not_found

(* construire_liste_lois l
 * l est une liste de string qui correspondent à un loisir
 * renvoie une liste de loisirs
 *)
let rec construire_liste_lois = function
	| [] -> []
	| "sport" :: tl -> Sport :: (construire_liste_lois tl)
	| "lecture" :: tl -> Lecture :: (construire_liste_lois tl)
	| "sorties" :: tl -> Sorties :: (construire_liste_lois tl)
	| "musique" :: tl -> Musique :: (construire_liste_lois tl)
	| "films" :: tl -> Films :: (construire_liste_lois tl)
	| "gastronomie" :: tl -> Gastronomie :: (construire_liste_lois tl)
	| "jeuxvideo" :: tl -> JeuxVideo :: (construire_liste_lois tl)
	| "artsplastiques" :: tl -> ArtsPlastiques :: (construire_liste_lois tl)
	| _ :: tl -> raise Not_found

(* ajout_adopteur id nom cheveux_l taille_min rel_l loisirs_l adopteurs_l
 * renvoie la liste adopteurs_l à laquelle on a ajoute un nouvel adopteur
 *)
let ajout_adopteur id nom ch taille_min rel_a lois_a ad_l =
		({id_c = id ;
		nom_c = nom ;
		cheveux_rech = (construire_liste_cheveux ch) ;
		taille_min = taille_min ;
		relation_recherchee_c = (construire_liste_rel rel_a) ;
		mes_loisirs_c = (construire_liste_lois lois_a) ; } :: ad_l)
		
(* ajout_adopte id nom cheveux taille rel_l loisirs_l adoptes_l
 * renvoie la liste adoptes_l à laquelle on a ajoute un nouvel adopte
 *)
let ajout_adopte id nom ch_a taille rel_a lois_a ad_l =
		({id_a = id ;
		nom_a = nom ;
		cheveux = begin match ch_a with
									| "brun" -> Brun
									| "blond" -> Blond
									| "roux" -> Roux
									| _ -> raise Not_found
							end ;
		taille = taille ;
		relation_recherchee_a = (construire_liste_rel rel_a) ;
		mes_loisirs_a = (construire_liste_lois lois_a) ; } :: ad_l)

(* lire_listes adopteurs_l adoptes_l
 * affiche chaque personne de chaque liste avec un résumé de son profil
 *)
let lire_listes l_adopteurs l_adoptes =
	Printf.printf "La liste des adopteurs est la suivante :\n\n" ;
	let rec lire_adopteurs = function
		| [] -> Printf.printf "\n\n" ;
		| adopteur :: tl -> Printf.printf "%s (id = %i) recherche un homme " adopteur.nom_c adopteur.id_c ;
												let rec lire_couleurs = function
													| [] -> Printf.printf ".\n"
													| Brun :: tl -> Printf.printf "brun " ; lire_couleurs tl
													| Blond :: tl -> Printf.printf "blond " ; lire_couleurs tl
													| Roux :: tl -> Printf.printf "roux " ; lire_couleurs tl
												in
												lire_couleurs adopteur.cheveux_rech ;
												Printf.printf "Il doit mesurer au moins %f m\n" adopteur.taille_min ;
												Printf.printf "%s recherche une relation " adopteur.nom_c ;
						 let rec lire_relations = function
						 	| [] -> Printf.printf ".\n"
							| Serieuse :: tl -> Printf.printf "serieuse " ; lire_relations tl
							| Amicale :: tl -> Printf.printf "amicale " ; lire_relations tl
							| Aventure :: tl -> Printf.printf "aventure " ; lire_relations tl
						 in
						 lire_relations adopteur.relation_recherchee_c ;
						 						Printf.printf "Ses loisirs sont " ;
						 let rec lire_loisirs = function
						 	| [] -> Printf.printf ".\n\n"
							| Sport :: tl -> Printf.printf "sport " ; lire_loisirs tl
							| Lecture :: tl -> Printf.printf "lecture " ; lire_loisirs tl
							| Sorties :: tl ->Printf.printf "sorties " ; lire_loisirs tl
							| Musique :: tl -> Printf.printf "musique " ; lire_loisirs tl
							| Films :: tl -> Printf.printf "films " ; lire_loisirs tl
							| Gastronomie :: tl -> Printf.printf "gastronomie " ; lire_loisirs tl
							| JeuxVideo :: tl -> Printf.printf "jeux video " ; lire_loisirs tl
							| ArtsPlastiques :: tl -> Printf.printf "arts plastiques " ; lire_loisirs tl						 in
						 lire_loisirs adopteur.mes_loisirs_c ;
						 						lire_adopteurs tl
	in
	lire_adopteurs l_adopteurs ;
	Printf.printf "La liste des adoptes est la suivante :\n\n" ;
	let rec lire_adoptes = function
		| [] -> Printf.printf "\n\n\n" ;
		| adopte :: tl -> Printf.printf "%s (id = %i) est " adopte.nom_a adopte.id_a ;
						  let lire_couleur = function
							| Brun -> Printf.printf "brun.\n"
							| Blond -> Printf.printf "blond.\n"
							| Roux -> Printf.printf "roux.\n"
						  in
						  lire_couleur adopte.cheveux ;
						  Printf.printf "Il mesure %f m\n" adopte.taille ;
						  Printf.printf "%s recherche une relation " adopte.nom_a ;
						  let rec lire_relations = function
						 	| [] -> Printf.printf ".\n"
							| Serieuse :: tl -> Printf.printf "serieuse " ; lire_relations tl
							| Amicale :: tl -> Printf.printf "amicale " ; lire_relations tl
							| Aventure :: tl -> Printf.printf "aventure " ; lire_relations tl
						  in
						  lire_relations adopte.relation_recherchee_a ;
						  Printf.printf "Ses loisirs sont " ;
						  let rec lire_loisirs = function
						 	| [] -> Printf.printf ".\n\n"
							| Sport :: tl -> Printf.printf "sport " ; lire_loisirs tl
							| Lecture :: tl -> Printf.printf "lecture " ; lire_loisirs tl
							| Sorties :: tl ->Printf.printf "sorties " ; lire_loisirs tl
							| Musique :: tl -> Printf.printf "musique " ; lire_loisirs tl
							| Films :: tl -> Printf.printf "films " ; lire_loisirs tl
							| Gastronomie :: tl -> Printf.printf "gastronomie " ; lire_loisirs tl
							| JeuxVideo :: tl -> Printf.printf "jeux video " ; lire_loisirs tl
							| ArtsPlastiques :: tl -> Printf.printf "arts plastiques " ; lire_loisirs tl						 in
						 lire_loisirs adopte.mes_loisirs_a ;
						 						lire_adoptes tl
	in
	lire_adoptes l_adoptes ;

