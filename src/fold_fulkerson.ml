open Graph
open Tools

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

let bfs graph s p =   (* TODO : verifier qu'on prend pas des arcs de flot nul *)
  let queue = Queue.create () in
  Queue.add s queue; 
  let paths = Array.make (Tools.size graph) [] in 
  let rec aux_bfs () = match Queue.take_opt queue with
  | None -> failwith "No path was found" (* Il n'y a pas de chemin *)
  | Some v when p = v -> paths.(p)  (* On renvoie le chemin *)
  | Some v -> let arcs_out = out_arcs_no_null graph v in 
      List.iter (fun arc -> match paths.(arc.tgt) with 
      | [] -> Queue.add arc.tgt queue; (* Pour chaque voisin non visité : l'ajouter à la file *)
              paths.(arc.tgt) <- v::paths.(v)  (* On ajoute v comme parent au chemin de ses voisins *) 
      | _ -> ()
    ) arcs_out;
    aux_bfs()
       (* Itération suivante *)
  in aux_bfs ()

let rec update_graph graph path flow_to_add = match path with
  | [] -> graph (* finir / en fait on arrive jamais ici ? sauf si c'est ce dont on part *)
  | _::[] -> graph (* on a vu tous les arcs => finir *)
  | x::y::p -> let graphXY = (add_arc graph x y flow_to_add) in let graphYX  = (add_arc graphXY y x (-flow_to_add)) in
    update_graph graphYX (y::p) flow_to_add
    

