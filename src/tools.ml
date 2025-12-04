open Graph

let clone_nodes gr = Graph.n_fold gr (fun g x -> Graph.new_node g x) Graph.empty_graph 

let gmap gr f = 
  e_fold gr  (fun g2 arc ->  (Graph.new_arc g2 {src = arc.src; tgt = arc.tgt; lbl = (f arc.lbl)}))  (clone_nodes gr)

let add_arc g id1 id2 n = match find_arc g id1 id2 with 
| None -> new_arc g {src = id1; tgt = id2; lbl = n}
| Some(arc) -> new_arc g {src = id1; tgt = id2; lbl = arc.lbl + n}

let size graph = 
  Graph.n_fold graph (fun acc _ -> acc + 1) 0
  