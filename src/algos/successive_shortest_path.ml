open Graph
open Graph_tools
open Algo_tools
open Bellman_ford

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
  let cost_graph = update_cost_graph (cost_graph input_graph) in

  let rec aux gap_graph =
    let path, _ =
      bellman_ford gap_graph cost_graph s p
    in
    match path with
    | [] ->
        gap_graph
    | _ ->
        let f = get_min_capa gap_graph path in
        display_list path;
        Printf.printf "\n";
        let new_graph = update_graph gap_graph path f in
        aux new_graph
  in
  flow_graph_no_parentheses main_graph (aux main_graph)
