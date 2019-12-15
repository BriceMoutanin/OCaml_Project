open Gfile
open Adoption
open Bfile
open Tools
open Fordfulkerson
open Graph
open Biparti
    
let () =

  (* Partie qui teste l'algorithme de Ford Fulkerson *)
  (*
  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;

  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)
  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)
  
  (* These command-line arguments are not used for the moment. *)
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in

  (* Open gfile *)
  let gr = from_file infile in
  let igr = gmap gr int_of_string in
  let ingr = fordfulk igr _source _sink in
  let ngr = gmap ingr string_of_int in
  let () = write_file outfile ngr in
  *)
  (****************************************************)

  (* Partie qui teste le projet *)

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 2 then
    begin
      Printf.printf "\nUsage: %s infile \n\n%!" Sys.argv.(0) ;
      exit 0
    end ;
  let infile = Sys.argv.(1) in

  (* Open bfile *)
  let (l1,l2) = from_bfile infile in
  let () = lire_listes l1 l2 in
  let () = matching l1 l2 in

  (***************************************************)
  
  ()
