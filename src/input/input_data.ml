open Graph
open Graph_tools

(*

let adapt_input_without_cost graph sources sinks = 
  let n = n_fold graph (fun n id -> n+1) 0 in
  let g = new_node (new_node graph (n+1)) (n+2) in 
*)

let get_sum_out_vertex_without_cost graph v = 
  let outs = out_arcs graph v in 
  List.fold_left (fun acc edge -> acc + edge.lbl) 0 outs


let get_sum_in_vertex_without_cost graph v = 
  e_fold graph (fun acc arc -> if arc.tgt = v then acc + arc.lbl else acc) 0



let adapt_input_without_cost graph sources sinks = 
  let n = size graph in
  let g = new_node (new_node graph (n)) (n+1) in 
  let g_with_sources = List.fold_left (fun graph id -> new_arc graph {src = n; tgt = id; lbl = (get_sum_out_vertex_without_cost graph id)}) g sources in 
  let g_with_sinks = List.fold_left (fun graph id -> new_arc graph {src = id; tgt = n+1; lbl = (get_sum_in_vertex_without_cost graph id)}) g_with_sources sinks in 
  g_with_sinks


let get_sum_out_vertex graph v = 
  let outs = out_arcs graph v in 
  List.fold_left (fun acc edge -> let capa,_ = edge.lbl in acc + capa) 0 outs 

let get_sum_in_vertex_with_cost graph v = 
  e_fold graph (fun acc arc -> let flow,_ = arc.lbl in if arc.tgt = v then acc + flow else acc) 0

let adapt_input_with_cost graph sources sinks = 
  let n = size graph in
  let g = new_node (new_node graph (n)) (n+1) in 
  let g_with_sources = List.fold_left (fun graph id -> new_arc graph {src = n; tgt = id; lbl = (get_sum_out_vertex graph id, 0)}) g sources in 
  let g_with_sinks = List.fold_left (fun graph id -> new_arc graph {src = id; tgt = n+1; lbl = (get_sum_in_vertex_with_cost graph id, 0)}) g_with_sources sinks in 
  g_with_sinks
  

let remove_additional_nodes graph = 
  let n = size graph - 1 in 
  let graph_with_node = n_fold graph (fun acc id -> if id < n-1 then (Printf.printf "adding node %d" id ; new_node acc id) else acc) empty_graph in 
  e_fold graph (fun acc edge -> if (edge.src >= n-1 || edge.tgt >= n-1) then acc else new_arc acc edge) graph_with_node