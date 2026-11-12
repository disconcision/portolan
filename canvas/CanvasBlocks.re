/* CanvasBlocks — see the interface. */
type placed = list((string, (float, float), float));

let pad = 46.; /* around a module's members, inside the room made for it */
let eps = 1.; /* nodes this close on an axis are in one column or row */

let in_root = (root: string, m_path: list(string)): bool =>
  switch (m_path) {
  | [r, ..._] => r == root
  | [] => false
  };

let base_keys = (g: CanvasGraph.t): list(string) =>
  switch (g.base) {
  | None => []
  | Some(b) => List.map((n: CanvasGraph.tynode) => n.key, b.nodes)
  };

let roots = (g: CanvasGraph.t): list(string) =>
  base_keys(g)
  |> List.filter(k =>
       List.exists((n: CanvasGraph.tynode) => in_root(k, n.m_path), g.nodes)
     )
  |> List.sort_uniq(compare);

let sub = (g: CanvasGraph.t, root: string): CanvasGraph.t => {
  let closed = base_keys(g);
  let held: Hashtbl.t(string, unit) = Hashtbl.create(64);
  let hold = k => Hashtbl.replace(held, k, ());
  let has = k => Hashtbl.mem(held, k);
  List.iter(
    (n: CanvasGraph.tynode) =>
      if (in_root(root, n.m_path)) {
        hold(n.key);
      },
    g.nodes,
  );
  let own_edges =
    List.filter((e: CanvasGraph.edge) => in_root(root, e.m_path), g.edges);
  /* what belongs to no module joins the one it hangs on: an end of one of
     its functions, a part of one of its types, a terminal docked to it */
  let loose =
    List.filter(
      (n: CanvasGraph.tynode) => n.m_path == [] && !List.mem(n.key, closed),
      g.nodes,
    );
  let hangs = (n: CanvasGraph.tynode) =>
    List.exists(
      (e: CanvasGraph.edge) => e.e_src == n.key || e.dst == n.key,
      own_edges,
    )
    || (
      switch (n.sat) {
      | Some((anchor, _)) => has(anchor)
      | None => false
      }
    )
    || List.exists(
         (m: CanvasGraph.tynode) => has(m.key) && List.mem(n.key, m.parts),
         g.nodes,
       );
  for (_ in 1 to 3) {
    List.iter(
      (n: CanvasGraph.tynode) =>
        if (!has(n.key) && hangs(n)) {
          hold(n.key);
        },
      loose,
    );
  };
  {
    ...g,
    nodes: List.filter((n: CanvasGraph.tynode) => has(n.key), g.nodes),
    edges:
      List.filter(
        (e: CanvasGraph.edge) => has(e.e_src) && has(e.dst),
        own_edges,
      ),
    values:
      List.filter(
        (v: CanvasGraph.value) => in_root(root, v.m_path) && has(v.v_key),
        g.values,
      ),
    loose_tests: [],
    all_tests: [],
    base: None,
    rim: [],
  };
};

let compose =
    (~base: placed, ~blocks: list((string, placed)))
    : list((string, (float, float))) => {
  let blocks =
    List.filter(
      ((root, own)) =>
        own != [] && List.exists(((k, _, _)) => k == root, base),
      blocks,
    );
  let is_block = k => List.mem_assoc(k, blocks);
  /* the room a module needs past the node it was: its members' box, padded */
  let room =
    List.map(
      ((root, own: placed)) => {
        let (l, r, t, b) =
          List.fold_left(
            ((l, r, t, b), (_, (x, y), rad)) =>
              (
                min(l, x -. rad),
                max(r, x +. rad),
                min(t, y -. rad),
                max(b, y +. rad),
              ),
            (infinity, neg_infinity, infinity, neg_infinity),
            own,
          );
        let (_, (cx, cy), r0) = List.find(((k, _, _)) => k == root, base);
        (
          root,
          ((cx, cy), r0, (l, t)),
          (
            max(0., r -. l +. 2. *. pad -. 2. *. r0),
            max(0., b -. t +. 2. *. pad -. 2. *. r0),
          ),
        );
      },
      blocks,
    );
  /* each open module pushes what is right of it right, what is below it down */
  let pushed = ((x, y): (float, float)): (float, float) =>
    List.fold_left(
      ((px, py), (_, ((cx, cy), _, _), (dx, dy))) =>
        (px +. (x > cx +. eps ? dx : 0.), py +. (y > cy +. eps ? dy : 0.)),
      (x, y),
      room,
    );
  let stay =
    List.filter_map(
      ((k, p, _)) => is_block(k) ? None : Some((k, pushed(p))),
      base,
    );
  let seen: Hashtbl.t(string, unit) = Hashtbl.create(64);
  let members =
    List.concat_map(
      ((root, own: placed)) => {
        let (_, (c, r0, (l, t)), _) =
          List.find(((r, _, _)) => r == root, room);
        let (ox, oy) = pushed(c);
        List.filter_map(
          ((k, (x, y), _)) =>
            if (Hashtbl.mem(seen, k)) {
              None;
            } else {
              Hashtbl.replace(seen, k, ());
              Some((
                k,
                (ox -. r0 +. pad +. x -. l, oy -. r0 +. pad +. y -. t),
              ));
            },
          own,
        );
      },
      blocks,
    );
  stay @ members;
};
