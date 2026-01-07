open Tools
open Graph
open Fold_fulkerson


let arc_cost (cost_graph : 'cost graph) (arc : 'flow arc) : 'cost =
  match find_arc cost_graph arc.src arc.tgt with
  | None -> (match find_arc cost_graph arc.tgt arc.src with 
    | None -> failwith "Là je comprends plus..."
    | Some a -> (- a.lbl)
  )
  | Some a -> a.lbl



let bellman_ford (graph : 'flow graph) (cost_graph : 'cost graph) (s : id) (p : id) : id list * int array =
  let n = size graph in
  let dist = Array.make n max_int in
  let parent = Array.make n (-1) in

  dist.(s) <- 0;

  (* relaxation |V|-1 fois *)
  for _ = 1 to n - 1 do
    for u = 0 to n - 1 do
      if dist.(u) < max_int then
        List.iter (fun arc ->
          if arc.lbl > 0 then begin
            let v = arc.tgt in
            let c = arc_cost cost_graph arc in
            if dist.(v) > dist.(u) + c then begin
              dist.(v) <- dist.(u) + c;
              parent.(v) <- u
            end
          end
        ) (out_arcs_no_null graph u)
    done
  done;

  (* reconstruction du chemin *)
  if dist.(p) = max_int then
    ([], dist)
  else
    let rec build_path v acc =
      if v = -1 then acc
      else build_path parent.(v) (v :: acc)
    in
    (build_path p [], dist)



let cost_graph (input_graph : ('flow * 'cost) graph) : 'cost graph = 
  gmap input_graph (fun (_,cost) -> cost)

let main_graph (input_graph : ('flow * 'cost) graph) : 'flow graph =
  gmap input_graph (fun (flow,_) -> flow)



  let successive_shortest_path
    (input_graph : ('flow * 'cost) graph)
    (s : id)
    (p : id)
  : string graph =

  let main_graph = main_graph input_graph in
  let cost_graph = cost_graph input_graph in

  let rec aux gap_graph =
    let path, _ =
      bellman_ford gap_graph cost_graph s p
    in
    match path with
    | [] ->
        gap_graph
    | _ ->
        let f = get_min_capa gap_graph path in
        let new_graph = update_graph gap_graph path f in
        aux new_graph
  in
  flow_graph_no_parentheses main_graph (aux main_graph)
