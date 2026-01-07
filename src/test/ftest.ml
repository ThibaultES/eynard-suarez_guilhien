open Gfile
open Graph_tools
(* open Fold_fulkerson *)
open Successive_shortest_path

(* Pour les tests *)
open Graph
let cost_graph_6 = [|1; 2; 3; 4; 3; 2; 1; 3; 4|]
let input_graph (graph : 'flow graph) cost_graph_list : ('flow * 'cost) graph =
  let i = ref (-1) in
  e_fold graph (fun acc_graph arc -> i := !i + 1 ;
    new_arc acc_graph {src = arc.src; tgt = arc.tgt; lbl = (arc.lbl, cost_graph_list.(!i))}
  ) (clone_nodes graph)
(* fin*)


let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf
        "\n ✻  Usage: %s infile source sink outfile\n\n%s%!" Sys.argv.(0)
        ("    🟄  infile  : input file containing a graph\n" ^
         "    🟄  source  : identifier of the source vertex (used by the ford-fulkerson algorithm)\n" ^
         "    🟄  sink    : identifier of the sink vertex (ditto)\n" ^
         "    🟄  outfile : output file in which the result should be written.\n\n") ;
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
  let graph = int_graph_of_string (from_file infile) in
  (* let gap_graph = ford_fulkerson graph 0 5 in *)
  let gap_graph = successive_shortest_path (input_graph graph cost_graph_6) 0 5 in


  (*
  let () = display_list (bfs graph _source _sink) in 
  let () = Printf.printf "La capacité minimal le long de ce chemin est %d" (get_min_capa graph (bfs graph _source _sink)) in

  (*A lancer sur graph2.txt *)
  let path1 = [0; 3; 8; 11; 9; 10; 12] in 
  let () = Printf.printf "La capacité minimal le long de ce chemin est %d" (get_min_capa graph path1) in
*)
  (* Rewrite the graph that has been read. *)
  let () = write_file outfile gap_graph in

  ()

