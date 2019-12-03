# OCaml_Project
Projet de OCaml

Ce projet est un système de matching entre plusieurs personnes qui recherchent différents types de relation.
Ce matching repose sur l'algorithme de Ford Fulkerson, afin de s'assurer que le plus grand nombre de personnes soit matché.
Il y a deux catégories à prendre en compte dans ce système :
- Les personnes déjà inscrites, qui ont renseigné leurs données et qui vont attendre d'être matchées :
  on les appelera les adoptés.
- Les personnes qui sont activement en recherche, et qui renseignent leurs différents critères de recherche :
  on les appelera les adopteurs.
  
Le système de matching se base sur différents critères :
- La couleur des cheveux
- La taille
- Les types de relation recherchée
- Les loisirs

| Comment ça marche ? |

Les différentes personnes sont définies dans un fichier texte selon le schéma suivant :

C Nom CouleursDeCheveuxRecherchées TailleMinimumAcceptée TypesRelationsRecherchées Loisirs

OU

A Nom CouleursDeCheveux Taille TypesRelationsRecherchées Loisirs

C = Adopteur
A = Adopté

La taille doit être un float.

Les couleurs de cheveux acceptées sont : blond, brun, ou roux.
Les types de relation acceptés sont : amicale, serieuse ou aventure.
Les différents loisirs acceptés sont : sport, lecture, films, gastronomie, musique, jeuxvideo, artsplastiques, ou sorties.

Voici un exemple :

C Amelie brun/blond 1.60 aventure/amicale/serieuse sport/films/gastronomie

A Antoine blond 1.94 amicale sorties/artsplastiques/lecture

| Comment l'utiliser ? |

Pour exécuter l'algorithme, il suffit d'exécuter le ftest avec en argument le fichier texte qui contient les différentes
personnes (selon le format expliqué précédemment).

Voici un exemple :

./ftest registre.txt

A l'éxécution, le programme doit réafficher un résumé de chacune des personnes impliquées dans le matching, puis donner tous
les matchs qui ont pu être faits (il affichera également tous les adopteurs qui n'auront pas pu être matchés).
