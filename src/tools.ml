open Graph

let clone_nodes gr = Graph.n_fold gr (fun g x -> Graph.new_node g x) Graph.empty_graph 

let gmap gr f = 
  e_fold gr  (fun g2 arc ->  (Graph.new_arc g2 {src = arc.src; tgt = arc.tgt; lbl = (f arc.lbl)}))  (clone_nodes gr)

let int_graph_of_string graph = gmap graph int_of_string

let add_arc g id1 id2 n = match find_arc g id1 id2 with 
| None -> new_arc g {src = id1; tgt = id2; lbl = n}
| Some(arc) -> new_arc g {src = id1; tgt = id2; lbl = arc.lbl + n}

let size graph = 
  Graph.n_fold graph (fun acc _ -> acc + 1) 0
  
let out_arcs_no_null graph v = 
  let rec aux list_arc acu = match list_arc with 
  |[] -> acu
  | arc::s when arc.lbl = 0 -> aux s acu
  | arc::s -> aux s (arc::acu) in 
  List.rev (aux (out_arcs graph v) []) 

let rec display_list list = match list with 
  | [] -> print_string "]"
  | a::s -> Printf.printf "%d, " a ; display_list s