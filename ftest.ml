open Gfile
open Tools
open Fordfulkerson
open Graph
    
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

  (*let gr5 = create_residual_graph gr4 in
  let () = write_file "graph5.txt" (gmap gr5 string_of_int) in

  let gr5_actu = fordfulk gr5 0 5 in
  let () = write_file "graph5_actu.txt" (gmap gr5_actu string_of_int) in*)
  
  let gr5 = new_node empty_graph 0 in
  let gr5 = new_node gr5 1 in
  let gr5 = new_node gr5 2 in
  let gr5 = new_node gr5 3 in
  let gr5 = new_node gr5 4 in
  let gr5 = new_node gr5 5 in
  let gr5 = new_node gr5 6 in
  let gr5 = new_arc gr5 0 1 4 in
  let gr5 = new_arc gr5 0 5 9 in
  let gr5 = new_arc gr5 1 2 2 in
  let gr5 = new_arc gr5 1 4 3 in
  let gr5 = new_arc gr5 5 4 8 in
  let gr5 = new_arc gr5 4 3 1 in
  let gr5 = new_arc gr5 2 3 4 in
  let gr5 = new_arc gr5 2 6 10 in
  let gr5 = new_arc gr5 3 6 1 in
  let gr5 = new_arc gr5 4 6 3 in
  let gr5 = create_residual_graph gr5 in
  let () = write_file "graph5.txt" (gmap gr5 string_of_int) in
  
  let gr5_actu = fordfulk gr5 0 6 in
  let () = write_file "graph5_actu.txt" (gmap gr5_actu string_of_int) in
  
  ()
