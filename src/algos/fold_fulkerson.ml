open Graph
open Tools





let test_graph graph =
  gmap graph (fun id -> (id / 2, id))


let flow_graph capa_graph gap_graph =
  e_fold gap_graph (fun g arc -> match find_arc capa_graph arc.src arc.tgt with
    | None -> g
    | Some arc_capa -> new_arc g {src = arc.src; tgt = arc.tgt; lbl = "("^(string_of_int (arc_capa.lbl - arc.lbl))^"/"^(string_of_int (arc_capa.lbl - arc_capa.lbl))^")"}
  ) (clone_nodes gap_graph)

let flow_graph_no_parentheses capa_graph gap_graph =
  e_fold gap_graph (fun g arc -> match find_arc capa_graph arc.src arc.tgt with
    | None -> g
    | Some arc_capa -> new_arc g {src = arc.src; tgt = arc.tgt; lbl = string_of_int (arc_capa.lbl - arc.lbl)}
  ) (clone_nodes gap_graph)



let get_min_capa graph path = 
  let rec aux_get_min_capa graph path mini = match path with 
  |[] -> failwith "path is empty"
  |[_] -> mini 
  |a::b::s -> (match (find_arc graph a b) with 
                | None -> failwith "should not happened"
                | Some(arc) when arc.lbl < mini -> aux_get_min_capa graph (b::s) arc.lbl  
                | Some(_) -> aux_get_min_capa graph (b::s) mini  
                )
in aux_get_min_capa graph path max_int



let rec update_graph graph path flow_to_add = match path with
  | [] -> graph (* finir / en fait on arrive jamais ici ? sauf si c'est ce dont on part *)
  | _::[] -> graph (* on a vu tous les arcs => finir *)
  | x::y::p -> let graphXY = (add_arc graph x y (-flow_to_add)) in let graphYX  = (add_arc graphXY y x flow_to_add) in
    update_graph graphYX (y::p) flow_to_add
    

let ford_fulkerson graph s p =
  let rec aux_fulkerson gap_graph = 
  let path = bfs gap_graph s p in match path with
    | [] -> gap_graph
    | _ -> aux_fulkerson (update_graph gap_graph path (get_min_capa gap_graph path))
in flow_graph_no_parentheses graph (aux_fulkerson graph) (* !!! flow_graph_no_parent !!!*)

