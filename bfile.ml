(* Le module Bfile a pour rôle de construire une structure de données
 * à partir d'un fichier texte au bon format.
 * Cette structure de données va contenir les différentes personnes impliquées
 * dans le matching
 *)

open Printf
open Adoption

type path = string

(* ////////////////////////////////////////////////////////////////////////////////////////////////////////////
 Format des fichiers texte
 % C = en recherche / A = en attente
 % Couleur de cheveux possibles : brun / blond / roux
 % Type de relation : aventure / amicale / serieuse
 % Loisirs possibles : sport / lecture / sorties / musique / films / gastronomie / jeux-video / arts-plastiques

 % Un adopteur est définit comme suivant :
 % (nom couleur_des_cheveux taille_min_acceptee type_relation liste_loisirs)
   C Amelie brun/blond/roux 1.20 aventure/amicale/serieuse sport/films/gastronomie

 % Un adopte est definit comme suivant :
 % (nom couleur_cheveux taille type_relation liste_loisirs)
   A Brice brun 1.77 aventure/serieuse sports/lecture/jeuxvideo
 * ////////////////////////////////////////////////////////////////////////////////////////////////////////////
 *)

(* read_adopteur n l_adopteurs line
 * renvoie la liste l_adopteurs à laquelle on a
 * rajouté un nouvel adopteur (id = n), construit
 * à partir de la line
 *)
let read_adopteur n l_adopteurs line =
	try Scanf.sscanf line "C %s %s %f %s %s" (fun nom ch_a taille_min_a rel_a lois_a -> ajout_adopteur n nom (String.split_on_char '/' ch_a) taille_min_a (String.split_on_char '/' rel_a) (String.split_on_char '/' lois_a) l_adopteurs)
	with e ->
    Printf.printf "Cannot read adopteur in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"

(* read_adopte n l_adoptes line
 * renvoie la liste l_adoptes à laquelle on a
 * rajouté un nouvel adopte (id = n), construit
 * à partir de la line
 *)
let read_adopte n l_adoptes line =
	try Scanf.sscanf line "A %s %s %f %s %s" (fun nom ch taille rel_a lois_a -> ajout_adopte n nom ch taille (String.split_on_char '/' rel_a) (String.split_on_char '/' lois_a) l_adoptes)
	with e ->
    Printf.printf "Cannot read adopte in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"
    
(* Reads a comment or fail. *)
let read_comment l_adopteurs line =
  try Scanf.sscanf line "%%s" l_adopteurs
  with _ ->
    Printf.printf "Unknown line:\n%s\n%!" line ;
    failwith "from_file"

(* from_file path
 * renvoie un tuple de listes
 * la première liste est la liste des adopteurs
 * la seconde liste est la liste des adoptes
 *)
let from_bfile path =

  let infile = open_in path in

  (* Read all lines until end of file. 
   * n is the current node counter. *)
  let rec loop n l_adopteurs l_adoptes =
    try
      let line = input_line infile in

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in

      let (n2, l_adopteurs2, l_adoptes2) =
        (* Ignore empty lines *)
        if line = "" then (n, l_adopteurs, l_adoptes)

        (* The first character of a line determines its content : n or e. *)
        else match line.[0] with
          | 'C' -> (n+1, (read_adopteur n l_adopteurs line), l_adoptes)
          | 'A' -> (n+1, l_adopteurs, (read_adopte n l_adoptes line))

          (* It should be a comment, otherwise we complain. *)
          | _ -> (n, (read_comment l_adopteurs line), l_adoptes)
      in      
      loop n2 l_adopteurs2 l_adoptes2

    with End_of_file -> (l_adopteurs,l_adoptes) (* Done *)
  in

  let final_listes = loop 1 [] [] in
  
  close_in infile ;
  final_listes