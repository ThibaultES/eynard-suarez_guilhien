open Tools
open Graph
open Fold_fulkerson
open BinaryHeap

let add_cost d c = d + c (* cas entier *)

let arc_cost (cost_graph : 'cost graph) (arc : 'flow arc) : 'cost =
  match find_arc cost_graph arc.src arc.tgt with
  | None -> (match find_arc cost_graph arc.tgt arc.src with 
    | None -> failwith "Là je comprends plus..."
    | Some a -> (- a.lbl)
  )
  | Some a -> a.lbl


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
      | Some (u, _) when u = p ->
          List.rev paths.(p) (* fin : renvoyer le chemin *)
      | Some (u, _) ->
          List.iter (fun arc ->
            if arc.lbl <> 0 then begin
              let v = arc.tgt in
              let alt = add_cost dist.(u) (arc_cost cost_graph arc) in

              (* soit v jamais visité, soit chemin amélioré *)
              if dist.(v) > alt then begin (* || paths.(u) = []*)
                dist.(v) <- alt;
                paths.(v) <- v :: paths.(u);
                add_heap heap (v, dist.(v))
              end
            end
          ) (out_arcs_no_null graph u);
          aux ()
  in
  aux ()

  (*
let dijkstra_johnson graph cost_graph potential s p =
  let n = size graph in
  let dist = Array.make n max_int in
  let parent = Array.make n (-1) in
  let heap = create_heap (arc_size graph) in

  dist.(s) <- 0;
  add_heap heap (s, 0);

  let rec loop () =
    match take_min_heap heap with
    | None -> ()
    | Some (u, du) ->
        if du > dist.(u) then loop ()
        else
          List.iter (fun arc ->
            if arc.lbl > 0 then begin
              let v = arc.tgt in
              let c = arc_cost cost_graph arc in
              let reduced_cost =
                c + potential.(u) - potential.(v)
              in
              (* invariant : reduced_cost >= 0 *)
              let alt = dist.(u) + reduced_cost in
              if alt < dist.(v) then begin
                dist.(v) <- alt;
                parent.(v) <- u;
                add_heap heap (v, alt)
              end
            end
          ) (out_arcs_no_null graph u);
          loop ()
  in
  loop ();

  if dist.(p) = max_int then
    ([], dist)
  else
    let rec build_path v acc =
      if v = -1 then acc
      else build_path parent.(v) (v :: acc)
    in
    (build_path p [], dist)
*)

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

(*
let update_potentials potential dist =
  Array.iteri (fun v d ->
    if d < max_int then
      potential.(v) <- potential.(v) + d
  ) dist

*)
(*
let successive_shortest_path input_graph s p =
  let main_graph = main_graph input_graph in
  let cost_graph = cost_graph input_graph in
  let n = size main_graph in
  let potential = Array.make n 0 in

  let rec aux gap_graph =
    let path, dist =
      dijkstra_johnson gap_graph cost_graph potential s p
    in
    match path with
    | [] -> gap_graph
    | _ ->
        update_potentials potential dist;
        let f = get_min_capa gap_graph path in
        let new_graph = update_graph gap_graph path f in
        aux new_graph
  in
  flow_graph_no_parent main_graph (aux main_graph)
  *)

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
        Printf.printf "La capa min est : %d" f;
        let new_graph = update_graph gap_graph path f in
        aux new_graph
  in
  flow_graph_no_parent main_graph (aux main_graph)