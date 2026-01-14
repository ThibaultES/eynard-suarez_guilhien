open Graph
open Graph_tools
open Algo_tools

let bellman_ford (graph : 'flow graph) (cost_graph : 'cost graph) (s : id) (p : id) : id list * int array =
  let n = size graph in
  let dist = Array.make n max_int in
  let parent = Array.make n (-1) in

  dist.(s) <- 0;

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