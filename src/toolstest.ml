open Gfile
open Tools
(*open Fold_fulkerson*)
    
let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 4 then
    begin
      Printf.printf
        "\n ✻  Usage: %s infile source sink outfile\n\n%s%!" Sys.argv.(0)
        ("    🟄  infile  : input file containing a graph\n" ^
         "    🟄  outfiles (3) : output file in which the result should be written.\n\n") ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)
  
  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(2)  
  and outfile2 = Sys.argv.(3) in

  (* Open file *)
  let graph = gmap (from_file infile) int_of_string in



  let clone_node_result = Tools.clone_nodes graph in 
  let add_arc_result = Tools.add_arc graph 1 2 5 in 

  (* Rewrite the graph that has been read. *)
  let () = write_file outfile clone_node_result in 
  let () = write_file outfile2 (gmap add_arc_result string_of_int)

  in 
  let () = export "graphs/test2.txt" (gmap graph string_of_int)
  
  (*
  in
  let () = export "graphs/testGAP.txt" (gmap (gap_graph graph) string_of_int)
  *)
  
in ()

