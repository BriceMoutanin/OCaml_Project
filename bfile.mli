(* Le module Bfile a pour r™le de construire une structure de donnŽes
 * ˆ partir d'un fichier texte au bon format.
 * Cette structure de donnŽes va contenir les diffŽrentes personnes impliquŽes
 * dans le matching
 *)

open Adoption

type path = string


(* from_file path
 * renvoie un tuple de listes
 * la premire liste est la liste des adopteurs
 * la seconde liste est la liste des adoptes
 *)
val from_bfile: path -> (adopteur list * adopte list)
