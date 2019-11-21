type couleur_cheveux = Brun | Blond | Roux
type relation = Serieuse | Aventure | Amicale
type loisirs = Sport | Lecture | Sorties | Musique | Films | Gastronomie | JeuxVideo | ArtsPlastiques

type adopteur = {
	id: int;
	nom: string;
	cheveux: couleur_cheveux list;
	taille_min: float;
	relation_recherchee: relation list;
	mes_loisirs: loisirs list;
}

type adopte = {
	id: int;
	nom: string;
	cheveux: couleur_cheveux;
	taille: float;
	relation_recherchee: relation list;
	mes_loisirs: loisirs list;	
}