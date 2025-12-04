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

let bfs graph s p =
  let q = Queue.add (Queue.create) s in 
  let paths = Array.make (Graph.size graph) [] in 
  let rec aux_bfs () = match Queue.take_opt queue with
  | None -> failwith "No path was found" (* Il n'y a pas de chemin *)
  | Some v when p = v -> paths.(p)  (* On renvoie le chemin *)
  | Some v -> let arcs_out = out_arcs graph v in
      List.iter (fun arc -> Queue.add queue arc; (* Pour chaque voisin non visité : l'ajouter à la file *)
                            v::paths.(arc.tgt)) arcs_out ; (* On ajoute v comme parent au chemin de ses voisins *) 
      aux_bfs() (* Itération suivante *)
  in aux_bfs graph q []
   
