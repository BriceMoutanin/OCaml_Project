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

val construire_liste_cheveux : string list -> couleur_cheveux list

val construire_liste_rel : string list -> relation list

val construire_liste_lois : string list -> loisirs list

val ajout_adopteur : int -> string -> string list -> float -> string list -> string list -> adopteur list -> adopteur list

val ajout_adopte : int -> string -> string -> float -> string list -> string list -> adopte list -> adopte list

val lire_listes : adopteur list -> adopte list -> unit