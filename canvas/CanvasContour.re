/* CanvasContour — see the interface. */
type t =
  | Arrows
  | Members
  | Smooth;

let styles = [
  ("arrows", "arrows"),
  ("members", "members"),
  ("smooth", "smooth"),
];
let current: ref(t) = ref(Arrows);
let style = (): t => current^;
let chosen = (): string =>
  switch (current^) {
  | Arrows => "arrows"
  | Members => "members"
  | Smooth => "smooth"
  };
let set = (k: string): bool => {
  let picked =
    switch (k) {
    | "arrows" => Some(Arrows)
    | "members" => Some(Members)
    | "smooth" => Some(Smooth)
    | _ => None
    };
  Option.iter(c => current := c, picked);
  picked != None;
};

let rec shared = (a: list(string), b: list(string)): list(string) =>
  switch (a, b) {
  | ([x, ...at], [y, ...bt]) when x == y => [x, ...shared(at, bt)]
  | _ => []
  };

let home =
    (nodes: list(CanvasGraph.tynode), edges: list(CanvasGraph.edge))
    : (string => list(string)) => {
  let own: Hashtbl.t(string, list(string)) = Hashtbl.create(64);
  let loose: Hashtbl.t(string, unit) = Hashtbl.create(16);
  List.iter(
    (n: CanvasGraph.tynode) => {
      Hashtbl.replace(own, n.key, n.m_path);
      if (n.kind == Builtin && n.m_path == []) {
        Hashtbl.replace(loose, n.key, ());
      };
    },
    nodes,
  );
  let used: Hashtbl.t(string, list(string)) = Hashtbl.create(16);
  List.iter(
    (e: CanvasGraph.edge) =>
      List.iter(
        k =>
          if (Hashtbl.mem(loose, k)) {
            Hashtbl.replace(
              used,
              k,
              switch (Hashtbl.find_opt(used, k)) {
              | Some(p) => shared(p, e.m_path)
              | None => e.m_path
              },
            );
          },
        [e.e_src, e.dst],
      ),
    edges,
  );
  k =>
    switch (Hashtbl.find_opt(used, k)) {
    | Some(p) => p
    | None => Option.value(~default=[], Hashtbl.find_opt(own, k))
    };
};

type point = (float, float);

let swell = 30.;

/* Prim's algorithm: the tree grows from the first circle by the nearest
   circle not yet in it. */
let links =
    (circles: list((string, point, float)))
    : list(((string, point), (string, point))) => {
  let c = Array.of_list(circles);
  let n = Array.length(c);
  let gap = (i: int, j: int): float => {
    let (_, (ax, ay), ra) = c[i]
    and (_, (bx, by), rb) = c[j];
    Float.hypot(ax -. bx, ay -. by) -. ra -. rb;
  };
  let joined = Array.make(n, false);
  /* each outside circle's nearest circle in the tree, and how near */
  let nearest = Array.make(n, 0);
  let dist = Array.make(n, Float.infinity);
  let out = ref([]);
  let add = (i: int) => {
    joined[i] = true;
    for (j in 0 to n - 1) {
      if (!joined[j] && gap(i, j) < dist[j]) {
        dist[j] = gap(i, j);
        nearest[j] = i;
      };
    };
  };
  if (n > 0) {
    add(0);
  };
  for (_ in 2 to n) {
    let next = ref(-1);
    for (j in 0 to n - 1) {
      if (!joined[j] && (next^ < 0 || dist[j] < dist[next^])) {
        next := j;
      };
    };
    let j = next^;
    if (dist[j] > 0.) {
      let (ia, a, _) = c[nearest[j]]
      and (ib, b, _) = c[j];
      out := [((ia, a), (ib, b)), ...out^];
    };
    add(j);
  };
  List.rev(out^);
};
