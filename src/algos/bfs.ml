open Graph
open Graph_tools

let bfs graph s p =
  let queue = Queue.create () in
  Queue.add s queue; 
  let paths = Array.make (Tools.size graph) [] in 
  let rec aux_bfs () = match Queue.take_opt queue with
  | None -> [] (* Il n'y a pas de chemin *)
  | Some v when p = v -> paths.(p) <- (p::paths.(p)); List.rev paths.(p)  (* On renvoie le chemin *)
  | Some v -> let arcs_out = out_arcs_no_null graph v in 
      List.iter (fun arc -> match paths.(arc.tgt) with 
      | [] -> Queue.add arc.tgt queue; (* Pour chaque voisin non visité : l'ajouter à la file *)
              paths.(arc.tgt) <- v::paths.(v)  (* On ajoute v comme parent au chemin de ses voisins *) 
      | _ -> ()
    ) arcs_out;
    aux_bfs()
       (* Itération suivante *)
  in aux_bfs ()