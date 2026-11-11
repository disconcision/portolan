/* The two region settings: what a contour is drawn around
   (CanvasContour) and where the pull leaves a module's members
   (CanvasPull), each on a handful of nodes placed by hand. */

module G = Portolan_graph.Graph;

let here: G.handle = {
  file: "lib/a.ml",
  path: "A",
  line: 1,
  end_line: 1,
};

let node = (~kind=G.Alias, ~m_path=[], key): G.node => {
  key,
  label: key,
  m_path,
  kind,
  ctors: [],
  n_ty: None,
  n_doc: None,
  n_err: false,
  parts: [],
  former: None,
  abstract: false,
  exported: true,
  at: kind == G.Alias ? Some(here) : None,
};

let edge = (~m_path, name, src, dst): G.edge => {
  e_name: name,
  e_label: name,
  e_m_path: m_path,
  e_ty: src ++ " -> " ++ dst,
  src,
  dst,
  e_doc: None,
  e_err: false,
  e_hole: false,
  main: false,
  e_deps: [],
  e_exported: true,
  tests: [],
  e_at: here,
};

let picture = (~edges=[], nodes): CanvasGraph.t =>
  CanvasGraph.of_graph({
    nodes,
    edges,
    values: [],
    loose_tests: [],
  });

let dist = ((ax, ay), (bx, by)) => Float.hypot(ax -. bx, ay -. by);
let centre = (ps: list((float, float))) => {
  let k = float_of_int(List.length(ps));
  (
    List.fold_left((s, (x, _)) => s +. x, 0., ps) /. k,
    List.fold_left((s, (_, y)) => s +. y, 0., ps) /. k,
  );
};
/* the widest distance between two of them */
let spread = (ps: list((float, float))) =>
  List.fold_left(
    (m, a) => List.fold_left((m, b) => max(m, dist(a, b)), m, ps),
    0.,
    ps,
  );

/* ---------- the contour ---------- */

let ids = links => List.map((((a, _), (b, _))) => (a, b), links);

let links_in_a_row = () =>
  Alcotest.(check(list(pair(string, string))))(
    "each joined to the next",
    [("a", "b"), ("b", "c")],
    ids(
      CanvasContour.links([
        ("a", (0., 0.), 10.),
        ("b", (100., 0.), 10.),
        ("c", (200., 0.), 10.),
      ]),
    ),
  );

let links_skip_overlaps = () =>
  Alcotest.(check(list(pair(string, string))))(
    "only the one that stands apart",
    [("b", "c")],
    ids(
      CanvasContour.links([
        ("a", (0., 0.), 60.),
        ("b", (100., 0.), 60.),
        ("c", (400., 0.), 60.),
      ]),
    ),
  );

let links_go_rim_to_rim = () =>
  /* c's centre is nearer to b's, but a is so large that its rim is nearer */
  Alcotest.(check(list(pair(string, string))))(
    "joined to the nearer rim",
    [("a", "b"), ("a", "c")],
    ids(
      CanvasContour.links([
        ("a", (0., 0.), 150.),
        ("b", (300., 0.), 10.),
        ("c", (200., 330.), 10.),
      ]),
    ),
  );

let shared_builtin = (paths: list(list(string))) => {
  let g =
    picture(
      ~edges=
        List.mapi(
          (i, m_path) =>
            edge(~m_path, "f" ++ string_of_int(i), "string", "bool"),
          paths,
        ),
      [node(~kind=G.Builtin, "string"), node(~kind=G.Builtin, "bool")],
    );
  CanvasContour.home(g.nodes, g.edges, "string");
};

let home_of_shared_builtin = () => {
  Alcotest.(check(list(string)))(
    "one module's functions share it: theirs",
    ["M"],
    shared_builtin([["M"], ["M"]]),
  );
  Alcotest.(check(list(string)))(
    "a nested module's and its parent's: the parent's",
    ["M"],
    shared_builtin([["M"], ["M", "N"]]),
  );
  Alcotest.(check(list(string)))(
    "two modules': nobody's",
    [],
    shared_builtin([["M"], ["P"]]),
  );
};

let home_of_named_type = () => {
  let g =
    picture(
      ~edges=[edge(~m_path=["P"], "f", "t", "t")],
      [node(~m_path=["M"], "t")],
    );
  Alcotest.(check(list(string)))(
    "its own module, whoever uses it",
    ["M"],
    CanvasContour.home(g.nodes, g.edges, "t"),
  );
};

/* ---------- the pull ---------- */

/* two modules of four types each, taking turns along a line */
let line = {
  let key = (m, i) => m ++ string_of_int(i);
  let g =
    picture(
      List.concat_map(
        i =>
          [
            node(~m_path=["A"], key("a", i)),
            node(~m_path=["B"], key("b", i)),
          ],
        [0, 1, 2, 3],
      ),
    );
  let placed =
    List.concat_map(
      i => {
        let x = float_of_int(i) *. 280.;
        [
          (key("a", i), (x, 0.), 20.),
          (key("b", i), (x +. 140., 0.), 20.),
        ];
      },
      [0, 1, 2, 3],
    );
  (g, placed);
};
let of_module = (m: string, ps: list((string, (float, float)))) =>
  List.filter_map(((k, p)) => k.[0] == m.[0] ? Some(p) : None, ps);

let pull_gathers = () => {
  let (g, placed) = line;
  let before = List.map(((k, p, _)) => (k, p), placed);
  let after = CanvasPull.apply(Strong, g, placed);
  Alcotest.(check(bool))(
    "module A is less spread out",
    true,
    spread(of_module("a", after)) < spread(of_module("a", before)),
  );
  Alcotest.(check(bool))(
    "the two no longer take turns: their centres are a module apart",
    true,
    dist(centre(of_module("a", after)), centre(of_module("b", after)))
    > 150.,
  );
};

let pull_keeps_modules_clear = () => {
  let (g, placed) = line;
  let after = CanvasPull.apply(Strong, g, placed);
  let nearest =
    List.fold_left(
      (m, a) =>
        List.fold_left(
          (m, b) => min(m, dist(a, b)),
          m,
          of_module("b", after),
        ),
      Float.infinity,
      of_module("a", after),
    );
  /* two region circles of 46 and the gap, less what snapping can take */
  Alcotest.(check(bool))(
    "no node of A within a region circle of a node of B",
    true,
    nearest >= 2. *. 46. +. 8. -. 14.,
  );
};

let pull_is_milder = () => {
  let (g, placed) = line;
  let a = s => spread(of_module("a", CanvasPull.apply(s, g, placed)));
  Alcotest.(check(bool))(
    "mild leaves module A wider than strong does",
    true,
    a(Mild) > a(Strong),
  );
};

let pull_stays_on_lattice = () => {
  let (g, placed) = line;
  let after = CanvasPull.apply(Strong, g, placed);
  let whole = v => Float.abs(v /. 14. -. Float.round(v /. 14.)) < 1e-6;
  Alcotest.(check(bool))(
    "every node moved by whole steps of 14",
    true,
    List.for_all2(
      ((_, (x0, y0), _), (_, (x1, y1))) =>
        whole(x1 -. x0) && whole(y1 -. y0),
      placed,
      after,
    ),
  );
};

/* two types of no module, nearly touching, and a function between them */
let pull_leaves_no_modules_alone = () => {
  let g =
    picture(
      ~edges=[edge(~m_path=[], "f", "t", "u")],
      [node("t"), node("u")],
    );
  let placed = [("t", (0., 0.), 20.), ("u", (42., 0.), 20.)];
  Alcotest.(check(list(pair(string, pair(float(1e-6), float(1e-6))))))(
    "where the layout put them",
    List.map(((k, p, _)) => (k, p), placed),
    CanvasPull.apply(Strong, g, placed),
  );
};

/* a ring of module A's types around one type of no module */
let pull_moves_a_stranger_out = () => {
  let ring = List.init(6, i => "a" ++ string_of_int(i));
  let g =
    picture([node("x"), ...List.map(k => node(~m_path=["A"], k), ring)]);
  let placed = [
    ("x", (0., 0.), 20.),
    ...List.mapi(
         (i, k) => {
           let t = float_of_int(i) *. Float.pi /. 3.;
           (k, (150. *. cos(t), 150. *. sin(t)), 20.);
         },
         ring,
       ),
  ];
  let after = CanvasPull.apply(Strong, g, placed);
  /* outside: seen from it, the members all lie within a half turn */
  let (x, y) = List.assoc("x", after);
  let towards =
    List.map(
      ((mx, my)) => Float.atan2(my -. y, mx -. x),
      of_module("a", after),
    )
    |> List.sort(compare);
  let rec widest = (first, angles) =>
    switch (angles) {
    | [a, b, ...rest] => max(b -. a, widest(first, [b, ...rest]))
    | [last] => first +. 2. *. Float.pi -. last
    | [] => 0.
    };
  Alcotest.(check(bool))(
    "the widest opening between the members, seen from it, is over a half turn",
    true,
    widest(List.hd(towards), towards) > Float.pi,
  );
};

/* A.t with two functions to int: A's own, and B's, whose other types are
   far to the right */
let terminals = {
  let g =
    picture(
      ~edges=[
        edge(~m_path=["A"], "own", "t", "int"),
        edge(~m_path=["B"], "theirs", "t", "int"),
      ],
      [
        node(~kind=G.Builtin, "int"),
        node(~m_path=["A"], "t"),
        node(~m_path=["A"], "u"),
        node(~m_path=["B"], "v"),
        node(~m_path=["B"], "w"),
      ],
    );
  let placed = [
    ("t", (0., 0.), 20.),
    ("u", (0., 280.), 20.),
    ("int@own", (70., (-42.)), 9.),
    ("int@theirs", (70., 42.), 9.),
    ("v", (1400., 0.), 20.),
    ("w", (1400., 280.), 20.),
  ];
  (g, placed);
};

let pull_keeps_a_modules_own_terminal_docked = () => {
  let (g, placed) = terminals;
  let after = CanvasPull.apply(Strong, g, placed);
  let (tx, ty) = List.assoc("t", after)
  and (ix, iy) = List.assoc("int@own", after);
  Alcotest.(check(pair(float(1e-6), float(1e-6))))(
    "where it was, relative to the type",
    (70., (-42.)),
    (ix -. tx, iy -. ty),
  );
};

let pull_lets_a_foreign_terminal_go_home = () => {
  let (g, placed) = terminals;
  let after = CanvasPull.apply(Strong, g, placed);
  let i = List.assoc("int@theirs", after);
  Alcotest.(check(bool))(
    "nearer to module B's types than to the type it was docked to",
    true,
    dist(i, List.assoc("v", after)) < dist(i, List.assoc("t", after)),
  );
};

let () = {
  let case = (name, f) => Alcotest.test_case(name, `Quick, f);
  Alcotest.run(
    "regions",
    [
      (
        "contour",
        [
          case("circles in a row are joined end to end", links_in_a_row),
          case("circles that overlap need no link", links_skip_overlaps),
          case("a link is measured rim to rim", links_go_rim_to_rim),
          case(
            "a shared built-in type is of the module that uses it",
            home_of_shared_builtin,
          ),
          case("a named type is of its own module", home_of_named_type),
        ],
      ),
      (
        "pull",
        [
          case("two modules that took turns are gathered", pull_gathers),
          case(
            "nodes of different modules end clear of each other",
            pull_keeps_modules_clear,
          ),
          case("mild gathers less than strong", pull_is_milder),
          case("nodes stay on the lattice", pull_stays_on_lattice),
          case(
            "a graph with no modules is left as it is",
            pull_leaves_no_modules_alone,
          ),
          case(
            "a node a module has closed around is moved out",
            pull_moves_a_stranger_out,
          ),
          case(
            "a module's own terminal stays docked",
            pull_keeps_a_modules_own_terminal_docked,
          ),
          case(
            "a terminal docked to another module's type goes home",
            pull_lets_a_foreign_terminal_go_home,
          ),
        ],
      ),
    ],
  );
};
