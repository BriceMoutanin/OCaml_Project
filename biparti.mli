(* Le module Biparti a pour but de construire un graphe biparti ˆ partir
 * de la structure de donnes construite au pralable, qui contient la liste
 * des adopteurs et des adoptes.
 * On appliquera enfin l'algorithme de FordFulkerson sur ce graphe biparti
 * pour obtenir le matching.
 *)

open Graph
open Adoption

(* renvoie le nombre de participants *)
val nb_participants : adopteur list -> adopte list -> int

(* renvoie une liste de paires possibles entre adopteurs et adoptes
 * Ces paires sont construites entre un adopteur et un adopte a condition :
 * - que la taille de l'adopte soit superieure taille minimale demande par l'adopteur
 * - qu'il existe une relation recherche en commun entre l'adopteur et l'adopte
 * - qu'il existe au moins un loisir en commun entre l'adopteur et l'adopte
 *)
val construire_paires : adopteur list -> adopte list -> (id * id) list

(* renvoie un graphe biparti grace aux paires construites avec construire_paires *)
val gr_biparti : adopteur list -> adopte list -> int graph

(* affiche a la console le matching obtenu a partir des listes des adopteurs
 * et des adoptes
 *)
val matching : adopteur list -> adopte list -> unit