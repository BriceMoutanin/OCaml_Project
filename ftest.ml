open Gfile
open Tools
open Fordfulkerson
    
let () =

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

  (* Open file *)
  let graph = from_file infile in

  (* Rewrite the graph that has been read. *)

  let gr4 = add_arc (gmap graph int_of_string) 0 1 3 in
  let () = write_file "graph4.txt" (gmap gr4 string_of_int) in
  
  let () = export "graphdot.txt" graph in

  let gr5 = create_residual_graph gr4 in
  let () = write_file "graph5.txt" (gmap gr5 string_of_int) in

  let chemin = find_path gr5 [] 0 5 in
  let () = Printf.printf "%s\n" (string_of_path chemin) in

  let flow_min_chemin = flow_min gr5 chemin in
  let () = Printf.printf "%i\n" flow_min_chemin in
  
  
  ()
