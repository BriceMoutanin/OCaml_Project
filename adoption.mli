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
val construire_liste_cheveux : string list -> couleur_cheveux list

(* construire_liste_rel l
 * l est une liste de string qui correspondent à un type de relation
 * renvoie une liste de relation
 *)
val construire_liste_rel : string list -> relation list

(* construire_liste_lois l
 * l est une liste de string qui correspondent à un loisir
 * renvoie une liste de loisirs
 *)
val construire_liste_lois : string list -> loisirs list

(* ajout_adopteur id nom cheveux_l taille_min rel_l loisirs_l adopteurs_l
 * renvoie la liste adopteurs_l à laquelle on a ajoute un nouvel adopteur
 *)
val ajout_adopteur : int -> string -> string list -> float -> string list -> string list -> adopteur list -> adopteur list

(* ajout_adopte id nom cheveux taille rel_l loisirs_l adoptes_l
 * renvoie la liste adoptes_l à laquelle on a ajoute un nouvel adopte
 *)
val ajout_adopte : int -> string -> string -> float -> string list -> string list -> adopte list -> adopte list

(* lire_listes adopteurs_l adoptes_l
 * affiche chaque personne de chaque liste avec un résumé de son profil
 *)
val lire_listes : adopteur list -> adopte list -> unit