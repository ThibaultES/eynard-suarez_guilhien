open Graph

let clone_nodes gr = Graph.n_fold gr (fun g x -> Graph.new_node g x) Graph.empty_graph 

let add_arc g id1 id2 n = match find_arc g id1 id2 with 
| None -> new_arc g {src = id1; tgt = id2; lbl = n}
| Some(arc) -> new_arc g {src = id1; tgt = id2; lbl = arc.lbl + n}