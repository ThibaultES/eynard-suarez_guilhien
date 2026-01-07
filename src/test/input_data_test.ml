(*open Files.Gfile
open Graph
open Graph_tools
open Input.Input_data


let input_graph (graph : 'flow graph) cost_graph_list : ('flow * 'cost) graph =
  let i = ref (-1) in
  e_fold graph (fun acc_graph arc -> i := !i + 1 ;
    new_arc acc_graph {src = arc.src; tgt = arc.tgt; lbl = (arc.lbl, cost_graph_list.(!i))}
  ) (clone_nodes graph)

let g_test = new_node (new_node (new_node (new_node (new_node empty_graph 0) 1) 2) 3) 4 

let g_test = new_arc g_test {src = 0; tgt = 1; lbl = (5,2)}  
let g_test = new_arc g_test {src = 0; tgt = 2; lbl = (3,1)} 
let g_test = new_arc g_test {src = 2; tgt = 3; lbl = (3,1)}  
let g_test = new_arc g_test {src = 1; tgt = 3; lbl = (3,1)} 
let g_test = new_arc g_test {src = 3; tgt = 4; lbl = (6,1)} 

let () = 

  if Array.length Sys.argv <> 3 then 
    begin
        Printf.printf
          "\n ✻  Usage: %s file_path name_out \n\n%s%!" Sys.argv.(0)
          ("    🟄  file_path  : input file containing a graph in txt format\n" ^
          "    🟄  name_out  : output file with in a valid format to be converted in svg");
        exit 0
      end ;

  (*Assuming graph6 *)
  let graph = from_file Sys.argv.(1) in 
  
  let cost_graph_6 = [|1; 2; 3; 4; 3; 2; 1; 3; 4|] in 

  let g6_with_cost = input_graph (gmap graph int_of_string) cost_graph_6 in 

  

  (*Tested with graph 10 *)
  let result = adapt_input_with_cost g6_with_cost [0;2] [3;5] in 

  let string_of_int_int (a, b) = Printf.sprintf "%d, %d" a b in 
  
  let result = remove_additional_nodes result in 
    
   export Sys.argv.(2) (gmap result string_of_int_int)



*)