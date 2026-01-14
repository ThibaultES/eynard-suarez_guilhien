open Graph_tools
open Graph

let gap_graph graph = (* gr : graphe dont les labels sont des tuples (flux, capacite) *)
  e_fold graph  ( 
    fun g2 arc ->  (match arc.lbl with
      | p, c when p = 0 && c > 0 -> Graph.new_arc g2 {src = arc.src; tgt = arc.tgt; lbl = c}
      | p, c when p = c && p > 0 -> Graph.new_arc g2 {src = arc.tgt; tgt = arc.src; lbl = p}
      | p, c when p > 0 -> let gnewarc = Graph.new_arc g2 {src = arc.src; tgt = arc.tgt; lbl = (c - p)} in
        Graph.new_arc gnewarc {src = arc.tgt; tgt = arc.src; lbl = p}
      | _,_ -> g2
      )
  )  (clone_nodes graph)

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

let arc_cost (cost_graph : 'cost graph) (arc : 'flow arc) : 'cost =
  match find_arc cost_graph arc.src arc.tgt with
  | None -> (match find_arc cost_graph arc.tgt arc.src with 
    | None -> failwith "Là je comprends plus..."
    | Some a -> (- a.lbl)
  )
  | Some a -> a.lbl

let rec update_graph graph path flow_to_add = match path with
  | [] -> graph (* finir / en fait on arrive jamais ici ? sauf si c'est ce dont on part *)
  | _::[] -> graph (* on a vu tous les arcs => finir *)
  | x::y::p -> let graphXY = (add_arc graph x y (-flow_to_add)) in let graphYX  = (add_arc graphXY y x flow_to_add) in
    update_graph graphYX (y::p) flow_to_add

let update_cost_graph cost_graph =
  e_fold cost_graph (fun g arc -> add_arc g arc.tgt arc.src (-arc.lbl)) (cost_graph)