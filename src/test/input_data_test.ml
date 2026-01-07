open Gfile
open Graph
open Tools
open Input_data


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

  (*Tested with graph 10 *)
  let result = adapt_input_with_cost g_test [0;1] [3;4] in 

  let string_of_int_int (a, b) = Printf.sprintf "%d, %d" a b in 


   export Sys.argv.(2) (gmap result string_of_int_int)

