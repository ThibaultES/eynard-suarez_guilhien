open Tools
open Graph
open Fold_fulkerson
open BinaryHeap

let arc_cost (cost_graph : 'cost graph) (arc : 'flow arc) (f : 'flow) : 'cost =
  match find_arc cost_graph arc.src arc.tgt with
  | None -> (match find_arc cost_graph arc.tgt arc.src with 
    | None -> failwith "Là je comprends plus..."
    | Some a -> a.lbl*f
  )
  | Some a -> a.lbl * f

let add_cost d c = d + c (* cas entier *)

let dijkstra_path (graph : 'flow graph) (cost_graph : 'cost graph) (s : id) (p : id) : id list =
  (* données *)
  let n = size graph in
  let dist = Array.make n max_int in
  let paths = Array.make n [] in
  let heap = create_heap (arc_size graph) in

  (* initialisation *)
  dist.(s) <- 0;
  paths.(s) <- [s];
  add_heap heap (s,0);

  let rec aux () =
    match take_min_heap heap with
      | None -> []   (* p non atteignable *)
      | Some (v, _) when v = p ->
          List.rev paths.(p) (* fin : renvoyer le chemin *)
      | Some (v, _) ->
          List.iter (fun arc ->
            if arc.lbl <> 0 then begin
              let u = arc.tgt in
              let alt = add_cost dist.(v) (arc_cost cost_graph arc arc.lbl) in

              (* soit u jamais visité, soit chemin amélioré *)
              if paths.(u) = [] || alt < dist.(u) then begin
                dist.(u) <- alt;
                paths.(u) <- u :: paths.(v);
                add_heap heap (u, dist.(u));
              end
            end
          ) (out_arcs_no_null graph v);
          aux ()
  in
  aux ()


let cost_graph (input_graph : ('flow * 'cost) graph) : 'cost graph = 
  gmap input_graph (fun (_,cost) -> cost)

let main_graph (input_graph : ('flow * 'cost) graph) : 'flow graph =
  gmap input_graph (fun (flow,_) -> flow)


let successive_shortest_path (input_graph : ('flow * 'cost) graph) (s : id) (p : id) =
  let main_graph = main_graph input_graph in
  let cost_graph = cost_graph input_graph in

  let rec aux_succ_short_path gap_graph = 
  let path = dijkstra_path gap_graph cost_graph s p in match path with
    | [] -> gap_graph
    | _ -> aux_succ_short_path (update_graph gap_graph path (get_min_capa gap_graph path))
in flow_graph_no_parent main_graph (aux_succ_short_path main_graph) (* !!! flow_graph_no_parent !!!*)
