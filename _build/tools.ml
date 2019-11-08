
let clone_nodes g = Graph.n_fold g Graph.new_node Graph.empty_graph

let gmap gr f =
  let gr2 = clone_nodes gr in
  Graph.e_fold gr (fun g id1 id2 a -> Graph.new_arc g id1 id2 (f a))  gr2

let add_arc g id1 id2 n =
  match (Graph.find_arc g id1 id2) with
  | Some lb -> Graph.new_arc g id1 id2 (lb+n)
  | None -> Graph.new_arc g id1 id2 n
