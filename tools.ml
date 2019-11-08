
let clone_nodes g = n_fold g new_node empty_graph

let gmap gr f =
  let gr2 = clone_nodes gr in
  e_fold gr (fun g id1 id2 a -> new_arc g id1 id2 (f a))  gr2

let add_arc g id1 id2 n =
	let g2 = clone_nodes g in
