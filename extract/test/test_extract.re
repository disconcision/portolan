/* Checks the graphs extracted from the example projects.

   Each example's annotations are built here the first time it is used,
   and the tests of a broken build copy an example to a temporary
   directory and build it there, so dune must be on the path. */

module G = Portolan_graph.Graph;
module Extract = Portolan_extract.Extract;
module Locate = Portolan_extract.Locate;

let build_dir = "_build_portolan";

/* The repository root: $PORTOLAN_ROOT, or the nearest directory above
   the working directory that holds the examples. Under [dune test] the
   working directory is _build/default/extract/test. */
let repo_root: string = (
  {
    let is_root = dir =>
      List.for_all(
        path => Sys.file_exists(List.fold_left(Filename.concat, dir, path)),
        [["examples", "vector", "dune-project"], ["portolan"]],
      );

    let rec up = dir =>
      if (is_root(dir)) {
        Some(dir);
      } else {
        let parent = Filename.dirname(dir);
        if (parent == dir) {
          None;
        } else {
          up(parent);
        };
      };

    let from_env =
      switch (Sys.getenv_opt("PORTOLAN_ROOT")) {
      | Some(dir) when is_root(dir) => Some(dir)
      | _ => None
      };

    switch (from_env, up(Sys.getcwd())) {
    | (Some(dir), _)
    | (None, Some(dir)) => dir
    | (None, None) =>
      prerr_endline(
        "test_extract: the examples were not found. Set PORTOLAN_ROOT to the repository.",
      );
      exit(1);
    };
  }: string
);

/* An example project, its annotations built (once per run). */
let example = {
  let built = Hashtbl.create(4);
  name => {
    let dir = List.fold_left(Filename.concat, repo_root, ["examples", name]);
    if (!Hashtbl.mem(built, name)) {
      Hashtbl.replace(built, name, ());
      let code =
        Sys.command(
          Printf.sprintf(
            "cd %s && dune build --root . --build-dir %s @check > /dev/null 2>&1",
            Filename.quote(dir),
            build_dir,
          ),
        );
      if (code != 0) {
        Printf.eprintf("test_extract: examples/%s does not build\n%!", name);
      };
    };
    dir;
  };
};

let extract = (~failing=?, name): G.t =>
  switch (
    Portolan_extract.Extract.extract(
      ~root=example(name),
      ~build_dir,
      ~failing,
      ~previous=None,
      ~broken=[],
    )
  ) {
  | Ok(g) => g
  | Error(message) => Alcotest.failf("extract %s: %s", name, message)
  };

let node = (g: G.t, key): G.node =>
  switch (List.find_opt((n: G.node) => n.key == key, g.nodes)) {
  | Some(n) => n
  | None => Alcotest.failf("no node %s", key)
  };

let edge = (g: G.t, name): G.edge =>
  switch (List.find_opt((e: G.edge) => e.e_name == name, g.edges)) {
  | Some(e) => e
  | None => Alcotest.failf("no edge %s", name)
  };

let value = (g: G.t, name): G.value =>
  switch (List.find_opt((v: G.value) => v.v_name == name, g.values)) {
  | Some(v) => v
  | None => Alcotest.failf("no value %s", name)
  };

let kind =
  Alcotest.testable(
    (ppf, k) => Format.pp_print_string(ppf, G.kind_to_string(k)),
    (==),
  );
let strings = Alcotest.(list(string));
let has_edge = (g: G.t, name) =>
  List.exists((e: G.edge) => e.e_name == name, g.edges);
let has_node = (g: G.t, key) =>
  List.exists((n: G.node) => n.key == key, g.nodes);
let in_file = (file, g: G.t): list(string) =>
  List.filter_map(
    (e: G.edge) => e.e_at.file == file ? Some(e.e_name) : None,
    g.edges,
  );
let test_names = (e: G.edge) => List.map((t: G.test) => t.t_name, e.tests);

/* ---------- examples/vector ---------- */

let vector_nodes = () => {
  let g = extract("vector");
  let point = node(g, "Scene.point");
  Alcotest.check(kind, "point is an alias", G.Alias, point.kind);
  Alcotest.(check(string))("point label", "point", point.label);
  Alcotest.check(strings, "point module", ["Scene"], point.m_path);
  Alcotest.(check(option(string)))(
    "point rhs",
    Some("int * int"),
    point.n_ty,
  );
  let shape = node(g, "Scene.shape");
  Alcotest.check(kind, "shape is an alias", G.Alias, shape.kind);
  Alcotest.check(
    strings,
    "shape ctors",
    ["Circle", "Rect", "Line"],
    shape.ctors,
  );
  Alcotest.(check(option(string)))(
    "shape doc",
    Some("A shape is a sum whose variants carry tuple payloads."),
    shape.n_doc,
  );
  Alcotest.(check(bool))("shape exported", true, shape.exported);
  Alcotest.(check(bool))("shape not abstract", false, shape.abstract);
  let scene = node(g, "Scene.scene");
  Alcotest.check(kind, "scene is an alias", G.Alias, scene.kind);
  Alcotest.(check(option(string)))(
    "scene rhs",
    Some("shape list"),
    scene.n_ty,
  );
  Alcotest.check(kind, "int is a builtin", G.Builtin, node(g, "int").kind);
  /* every key is used once, and everything an edge points to exists */
  let keys = List.map((n: G.node) => n.key, g.nodes);
  Alcotest.(check(int))(
    "keys are unique",
    List.length(keys),
    List.length(List.sort_uniq(compare, keys)),
  );
  List.iter(
    (e: G.edge) => {
      ignore(node(g, e.src));
      ignore(node(g, e.dst));
    },
    g.edges,
  );
};

let vector_edges = () => {
  let g = extract("vector");
  Alcotest.check(
    strings,
    "edges, in source order",
    [
      "Scene.area",
      "Scene.translate",
      "Scene.scale",
      "Scene.total_area",
      "Scene.move_all",
      "Scene.bounding_box",
    ],
    List.map((e: G.edge) => e.e_name, g.edges),
  );
  let area = edge(g, "Scene.area");
  Alcotest.(check(string))("area type", "shape -> int", area.e_ty);
  Alcotest.(check(string))("area src", "Scene.shape", area.src);
  Alcotest.(check(string))("area dst", "int", area.dst);
  Alcotest.(check(string))("area label", "area", area.e_label);
  Alcotest.check(strings, "area module", ["Scene"], area.e_m_path);
  Alcotest.(check(option(string)))(
    "area doc",
    Some("Twice the area, so everything stays in the integers."),
    area.e_doc,
  );
  Alcotest.(check(string))("area file", "lib/scene.ml", area.e_at.file);
  Alcotest.(check(string))("area path", "Scene/area", area.e_at.path);
  Alcotest.(check(pair(int, int)))(
    "area lines",
    (14, 18),
    (area.e_at.line, area.e_at.end_line),
  );
  let translate = edge(g, "Scene.translate");
  let src = node(g, translate.src);
  Alcotest.check(kind, "translate src is a product", G.Product, src.kind);
  Alcotest.check(
    strings,
    "translate src parts",
    ["Scene.point", "Scene.shape"],
    src.parts,
  );
  Alcotest.(check(string))("translate dst", "Scene.shape", translate.dst);
  let scale = edge(g, "Scene.scale");
  Alcotest.check(
    strings,
    "scale src parts",
    ["int", "Scene.shape"],
    node(g, scale.src).parts,
  );
  Alcotest.(check(string))(
    "total_area src",
    "Scene.scene",
    edge(g, "Scene.total_area").src,
  );
  Alcotest.check(
    strings,
    "total_area deps",
    ["Scene.area"],
    edge(g, "Scene.total_area").e_deps,
  );
  Alcotest.check(
    strings,
    "move_all deps",
    ["Scene.translate"],
    edge(g, "Scene.move_all").e_deps,
  );
  let bbox = edge(g, "Scene.bounding_box");
  Alcotest.(check(bool))("bounding_box is a hole", true, bbox.e_hole);
  let dst = node(g, bbox.dst);
  Alcotest.check(kind, "bounding_box dst is derived", G.Derived, dst.kind);
  Alcotest.check(
    strings,
    "bounding_box dst parts",
    ["Scene.point", "Scene.point"],
    dst.parts,
  );
  Alcotest.(check(option(string)))(
    "bounding_box dst is a tuple",
    Some("()"),
    dst.former,
  );
  Alcotest.check(
    strings,
    "only bounding_box is a hole",
    ["Scene.bounding_box"],
    List.filter_map(
      (e: G.edge) =>
        if (e.e_hole) {
          Some(e.e_name);
        } else {
          None;
        },
      g.edges,
    ),
  );
  let origin = value(g, "Scene.origin");
  Alcotest.(check(string))("origin key", "Scene.point", origin.v_key);
  Alcotest.(check(int))("one value", 1, List.length(g.values));
};

let vector_tests = () => {
  let g = extract("vector");
  let area = edge(g, "Scene.area");
  Alcotest.check(
    strings,
    "area tests",
    ["area of a rect", "area of a line"],
    test_names(area),
  );
  Alcotest.check(
    strings,
    "translate tests",
    ["translate moves a circle"],
    test_names(edge(g, "Scene.translate")),
  );
  Alcotest.check(
    strings,
    "total_area tests",
    ["total area adds up"],
    test_names(edge(g, "Scene.total_area")),
  );
  Alcotest.check(
    strings,
    "scale tests",
    [],
    test_names(edge(g, "Scene.scale")),
  );
  Alcotest.(check(int))("no loose tests", 0, List.length(g.loose_tests));
  Alcotest.(check(bool))("area is main", true, area.main);
  Alcotest.(check(bool))(
    "scale is not main",
    false,
    edge(g, "Scene.scale").main,
  );
  let first = List.hd(area.tests);
  Alcotest.check(strings, "test refs", ["Scene.area"], first.t_refs);
  Alcotest.(check(int))("test line", 20, first.t_at.line);
  Alcotest.(check(bool))(
    "unknown without a run",
    true,
    List.for_all((t: G.test) => t.t_status == G.Unknown, area.tests),
  );
};

let vector_failing = () => {
  let g = extract(~failing=[("lib/scene.ml", 21)], "vector");
  let statuses = (e: G.edge) =>
    List.map((t: G.test) => t.t_status, e.tests);
  Alcotest.(check(bool))(
    "line 21 fails, line 20 passes",
    true,
    statuses(edge(g, "Scene.area")) == [G.Pass, G.Fail],
  );
  Alcotest.(check(bool))(
    "the others pass",
    true,
    statuses(edge(g, "Scene.translate")) == [G.Pass],
  );
};

let deterministic = () =>
  List.iter(
    name =>
      Alcotest.(check(string))(
        name,
        G.to_string(extract(name)),
        G.to_string(extract(name)),
      ),
    ["vector", "shapes", "plane", "checks"],
  );

/* ---------- examples/shapes ---------- */

let shapes_interface = () => {
  let g = extract("shapes");
  let length = node(g, "Geo.length");
  Alcotest.(check(bool))("Geo.length is abstract", true, length.abstract);
  Alcotest.(check(bool))("Geo.length is exported", true, length.exported);
  Alcotest.(check(option(string)))(
    "doc taken from the interface",
    Some("A length in millimetres. Its representation is hidden."),
    length.n_doc,
  );
  let vec = node(g, "Geo.vec");
  Alcotest.(check(bool))("Geo.vec is not abstract", false, vec.abstract);
  Alcotest.check(strings, "Geo.vec fields", ["x", "y"], vec.ctors);
  Alcotest.(check(bool))(
    "Figure.t (no interface) is exported",
    true,
    node(g, "Figure.t").exported,
  );
  Alcotest.(check(bool))(
    "Figure.t is not abstract",
    false,
    node(g, "Figure.t").abstract,
  );
  let exported = name => edge(g, name).e_exported;
  Alcotest.(check(bool))("Geo.mm is exported", true, exported("Geo.mm"));
  Alcotest.(check(bool))(
    "Geo.clamp is private",
    false,
    exported("Geo.clamp"),
  );
  Alcotest.(check(bool))(
    "Geo.Angle.to_deg is exported",
    true,
    exported("Geo.Angle.to_deg"),
  );
  Alcotest.(check(bool))(
    "Geo.Angle.normalise is private",
    false,
    exported("Geo.Angle.normalise"),
  );
  Alcotest.(check(bool))(
    "Figure.shift is exported",
    true,
    exported("Figure.shift"),
  );
  Alcotest.(check(option(string)))(
    "edge doc from the interface",
    Some("Component-wise sum."),
    edge(g, "Geo.add").e_doc,
  );
};

let shapes_modules = () => {
  let g = extract("shapes");
  let angle = node(g, "Geo.Angle.t");
  Alcotest.check(
    strings,
    "nested type module path",
    ["Geo", "Angle"],
    angle.m_path,
  );
  Alcotest.(check(string))("nested type label", "t", angle.label);
  Alcotest.check(strings, "nested type ctors", ["Deg", "Turn"], angle.ctors);
  let to_deg = edge(g, "Geo.Angle.to_deg");
  Alcotest.check(
    strings,
    "nested edge module path",
    ["Geo", "Angle"],
    to_deg.e_m_path,
  );
  Alcotest.(check(string))(
    "nested edge handle",
    "Geo/Angle/to_deg",
    to_deg.e_at.path,
  );
  Alcotest.(check(string))(
    "nested edge file",
    "lib/geo.ml",
    to_deg.e_at.file,
  );
  Alcotest.(check(string))("nested edge src", "Geo.Angle.t", to_deg.src);
  Alcotest.check(
    strings,
    "deps inside a submodule",
    ["Geo.Angle.normalise"],
    to_deg.e_deps,
  );
  /* across files */
  Alcotest.check(
    strings,
    "Figure.length deps",
    ["Geo.mm", "Geo.norm1", "Geo.Angle.to_deg"],
    edge(g, "Figure.length").e_deps,
  );
  Alcotest.check(
    strings,
    "Figure.shift deps",
    ["Geo.add"],
    edge(g, "Figure.shift").e_deps,
  );
  Alcotest.(check(string))(
    "a type from another file",
    "Geo.length",
    edge(g, "Figure.length").dst,
  );
  Alcotest.(check(string))(
    "printed from where it is used",
    "t -> Geo.length",
    edge(g, "Figure.length").e_ty,
  );
  Alcotest.(check(bool))("Geo.mm is main", true, edge(g, "Geo.mm").main);
};

let shapes_arguments = () => {
  let g = extract("shapes");
  let parts = name => node(g, edge(g, name).src).parts;
  Alcotest.check(
    strings,
    "labelled argument",
    ["Geo.Angle.t", "Figure.t"],
    parts("Figure.rotate"),
  );
  Alcotest.check(
    strings,
    "optional argument",
    ["string", "Figure.t"],
    parts("Figure.describe"),
  );
  Alcotest.check(
    strings,
    "same type twice",
    ["Geo.vec", "Geo.vec"],
    parts("Geo.add"),
  );
  let outline = edge(g, "Figure.outline");
  Alcotest.(check(bool))("assert false is a hole", true, outline.e_hole);
  Alcotest.check(
    kind,
    "t list is derived",
    G.Derived,
    node(g, outline.src).kind,
  );
  let formed = key => (node(g, key).former, node(g, key).parts);
  let former = Alcotest.(pair(option(string), strings));
  Alcotest.check(
    former,
    "t list is a list of t",
    (Some("[]"), ["Figure.t"]),
    formed(outline.src),
  );
  Alcotest.check(
    former,
    "an option of a pair is an option of one node",
    (Some("?"), ["Geo.vec * Geo.vec"]),
    formed(outline.dst),
  );
  Alcotest.check(
    former,
    "the pair is a tuple of two vecs",
    (Some("()"), ["Geo.vec", "Geo.vec"]),
    formed("Geo.vec * Geo.vec"),
  );
  Alcotest.(check(string))(
    "value",
    "Figure.t",
    value(g, "Figure.unit_dot").v_key,
  );
};

let shapes_test_functions = () => {
  let g = extract("shapes");
  Alcotest.check(
    strings,
    "let test_shift_dot ()",
    ["shift_dot"],
    test_names(edge(g, "Figure.shift")),
  );
  Alcotest.check(
    strings,
    "let test_length_of_dot ()",
    ["length_of_dot"],
    test_names(edge(g, "Figure.length")),
  );
  Alcotest.(check(bool))(
    "a test function is not an edge",
    false,
    List.exists((e: G.edge) => e.e_name == "Figure.test_shift_dot", g.edges),
  );
};

/* ---------- examples/checks ---------- */

let tests_of = (g: G.t): list(G.test) =>
  List.concat_map((e: G.edge) => e.tests, g.edges) @ g.loose_tests;

let test = (g: G.t, name): G.test =>
  switch (List.find_opt((t: G.test) => t.t_name == name, tests_of(g))) {
  | Some(t) => t
  | None => Alcotest.failf("no test %s", name)
  };

let has_test = (g: G.t, edge_name, name) =>
  List.mem(name, test_names(edge(g, edge_name)));

/* Alcotest cases are found, and attached to what they exercise. */
let checks_cases = () => {
  let g = extract("checks");
  let on = (what, edge_name, name) =>
    Alcotest.(check(bool))(what, true, has_test(g, edge_name, name));
  on(
    "the body is a function of the test file",
    "Tally.count",
    "tally: counts",
  );
  on("through a wrapper of test_case", "Tally.top", "tally: top");
  on("through a helper of the test file", "Tally.add", "tally: adds");
  on("the body is written in place", "Tally.total", "tally: total");
  on("outside a suite: no suite in its name", "Window.inside", "inside");
  on("through a value of the test file", "Tally.add", "tally: counts");
  Alcotest.check(
    strings,
    "one that reaches nothing outside its file stays in it",
    ["Test_tally.twice"],
    test(g, "tally: doubles").t_refs,
  );
  Alcotest.check(
    strings,
    "inline tests are still there",
    ["<<(total empty) = 0>>", "tally: total"],
    test_names(edge(g, "Tally.total")),
  );
  Alcotest.(check(int))("none is loose", 0, List.length(g.loose_tests));
  /* a function that is a case's body is that test, not an edge */
  Alcotest.check(
    strings,
    "the edges of the test file",
    ["Test_tally.seen", "Test_tally.twice", "Test_tally.quick"],
    in_file("test/test_tally.ml", g),
  );
  let counts = test(g, "tally: counts");
  Alcotest.(check(string))(
    "a case is where its function is",
    "Test_tally/counts",
    counts.t_at.path,
  );
  Alcotest.(check(pair(int, int)))(
    "its lines",
    (10, 10),
    (counts.t_at.line, counts.t_at.end_line),
  );
  let total = test(g, "tally: total");
  Alcotest.(check(string))(
    "or where it is written",
    "Test_tally/tally: total",
    total.t_at.path,
  );
  Alcotest.(check(pair(int, int)))(
    "its lines",
    (41, 42),
    (total.t_at.line, total.t_at.end_line),
  );
  Alcotest.(check(bool))(
    "unknown without a run",
    true,
    List.for_all((t: G.test) => t.t_status == G.Unknown, tests_of(g)),
  );
};

let status =
  Alcotest.testable(
    (ppf, s) => Format.pp_print_string(ppf, G.status_to_string(s)),
    (==),
  );

let checks_failing = () => {
  let after = failing => {
    let g = extract(~failing, "checks");
    name => test(g, name).t_status;
  };
  /* [FAIL] tally 1 adds. */
  let of_ = after([("alcotest:tally", 1)]);
  Alcotest.check(
    status,
    "the second case of tally",
    G.Fail,
    of_("tally: adds"),
  );
  Alcotest.check(status, "the first passes", G.Pass, of_("tally: counts"));
  Alcotest.check(
    status,
    "an inline test passes",
    G.Pass,
    of_("<<(total empty) = 0>>"),
  );
  Alcotest.check(
    status,
    "a case outside a suite passes",
    G.Pass,
    of_("inside"),
  );
  /* File "test/test_tally.ml", line 16: a location inside a case */
  let of_ = after([("test/test_tally.ml", 16)]);
  Alcotest.check(
    status,
    "a location in its function",
    G.Fail,
    of_("tally: top"),
  );
  Alcotest.check(status, "the others pass", G.Pass, of_("tally: adds"));
  let of_ = after([("test/test_tally.ml", 42)]);
  Alcotest.check(
    status,
    "a location in its text",
    G.Fail,
    of_("tally: total"),
  );
  /* [FAIL] window 0 inside: which case that is, is not written down */
  let of_ = after([("alcotest:window", 0)]);
  Alcotest.check(
    status,
    "it may be the one outside a suite",
    G.Unknown,
    of_("inside"),
  );
  Alcotest.check(
    status,
    "it is none of tally's",
    G.Pass,
    of_("tally: counts"),
  );
};

/* What a ppx writes has no place of its own: it is where its type is. */
let checks_generated = () => {
  let g = extract("checks");
  let lines = name => {
    let at = edge(g, name).e_at;
    (at.line, at.end_line);
  };
  Alcotest.(check(pair(int, int)))(
    "equal_entry is at type entry",
    (6, 6),
    lines("Tally.equal_entry"),
  );
  Alcotest.(check(pair(int, int)))(
    "equal is at type t",
    (8, 8),
    lines("Tally.equal"),
  );
};

/* Unnamed types are formed from their parts, one level at a time. */
let checks_formers = () => {
  let g = extract("checks");
  let check = (key, former, parts) => {
    let n = node(g, key);
    Alcotest.(check(option(string)))(key ++ " former", former, n.former);
    Alcotest.check(strings, key ++ " parts", parts, n.parts);
  };
  check("Tally.out", Some("="), ["Buffer.t"]);
  check("Tally.entry -> bool", Some("->"), ["Tally.entry", "bool"]);
  check("(Tally.t, string) result", None, ["Tally.t", "string"]);
  check("Tally.entry option", Some("?"), ["Tally.entry"]);
  check("string list", Some("[]"), ["string"]);
  Alcotest.check(
    kind,
    "a function type is derived",
    G.Derived,
    node(g, "Tally.entry -> bool").kind,
  );
};

let errors = () =>
  switch (
    Portolan_extract.Extract.extract(
      ~root=example("vector"),
      ~build_dir="no_such_dir",
      ~failing=None,
      ~previous=None,
      ~broken=[],
    )
  ) {
  | Ok(_) => Alcotest.fail("expected an error")
  | Error(_) => ()
  };

/* ---------- the record: JSON, formers, values ---------- */

let examples = ["vector", "shapes", "plane", "checks"];

/* Every key is used once; nothing points at a node or a name that is
   not there. */
let check_well_formed = (what: string, g: G.t) => {
  let keys = List.map((n: G.node) => n.key, g.nodes);
  let edge_names = List.map((e: G.edge) => e.e_name, g.edges);
  let names = edge_names @ List.map((v: G.value) => v.v_name, g.values);
  let unique = (kind, l) =>
    Alcotest.(check(int))(
      Printf.sprintf("%s: %s are unique", what, kind),
      List.length(l),
      List.length(List.sort_uniq(compare, l)),
    );
  unique("node keys", keys);
  unique("definition names", names);
  let has = (kind, among, from, x) =>
    if (!List.mem(x, among)) {
      Alcotest.failf(
        "%s: %s %s of %s is not in the graph",
        what,
        kind,
        x,
        from,
      );
    };
  List.iter(
    (n: G.node) => List.iter(has("part", keys, n.key), n.parts),
    g.nodes,
  );
  let test = (t: G.test) =>
    List.iter(has("ref", edge_names, t.t_name), t.t_refs);
  List.iter(
    (e: G.edge) => {
      has("src", keys, e.e_name, e.src);
      has("dst", keys, e.e_name, e.dst);
      List.iter(has("dep", names, e.e_name), e.e_deps);
      List.iter(test, e.tests);
    },
    g.edges,
  );
  List.iter(
    (v: G.value) => {
      has("key", keys, v.v_name, v.v_key);
      List.iter(has("dep", names, v.v_name), v.v_deps);
    },
    g.values,
  );
  List.iter(test, g.loose_tests);
};

let handle: G.handle = {
  file: "lib/a.ml",
  path: "A/x",
  line: 3,
  end_line: 5,
};

/* one of everything [to_json] can write */
let hand_made: G.t = {
  let test = (name, status): G.test => {
    t_name: name,
    t_status: status,
    t_refs: ["A.f"],
    t_at: handle,
  };
  {
    nodes: [
      {
        key: "A.t",
        label: "t",
        m_path: ["A"],
        kind: Alias,
        ctors: ["x", "y"],
        n_ty: Some("{ x : int; y : int; }"),
        n_doc: Some("a \"doc\"\nover two lines"),
        n_err: true,
        parts: ["int", "int"],
        former: Some("{}"),
        abstract: true,
        exported: false,
        at: Some(handle),
      },
      ...List.map(
           (kind): G.node =>
             {
               key: G.kind_to_string(kind),
               label: "",
               m_path: [],
               kind,
               ctors: [],
               n_ty: None,
               n_doc: None,
               n_err: false,
               parts: [],
               former: None,
               abstract: false,
               exported: true,
               at: None,
             },
           [G.Builtin, G.Derived, G.Ghost, G.Product],
         ),
    ],
    edges: [
      {
        e_name: "A.f",
        e_label: "f",
        e_m_path: ["A"],
        e_ty: "t -> int",
        src: "A.t",
        dst: "builtin",
        e_doc: Some("doc"),
        e_err: true,
        e_hole: true,
        main: true,
        e_deps: ["A.x"],
        e_exported: false,
        tests: [test("passes", Pass), test("fails", Fail)],
        e_at: handle,
      },
    ],
    values: [
      {
        v_name: "A.x",
        v_label: "x",
        v_m_path: ["A"],
        v_key: "A.t",
        v_ty: "t",
        v_doc: Some("doc"),
        v_err: true,
        v_deps: ["A.f"],
        v_exported: false,
        v_at: handle,
      },
    ],
    loose_tests: [
      {
        ...test("loose", Unknown),
        t_refs: [],
      },
    ],
  };
};

let json_round_trip = () => {
  let check = (what, g: G.t) => {
    Alcotest.(check(bool))(
      what ++ ": of_json inverts to_json",
      true,
      G.of_json(G.to_json(g)) == Ok(g),
    );
    Alcotest.(check(bool))(
      what ++ ": of_string inverts to_string",
      true,
      G.of_string(G.to_string(g)) == Ok(g),
    );
    switch (G.of_string(G.to_string(g))) {
    | Ok(g') =>
      Alcotest.(check(string))(
        what ++ ": and back",
        G.to_string(g),
        G.to_string(g'),
      )
    | Error(message) => Alcotest.fail(message)
    };
  };
  check("hand-made", hand_made);
  check("empty", G.empty);
  List.iter(name => check(name, extract(name)), examples);
};

let json_errors = () => {
  let is_error = (what, text) =>
    switch (G.of_string(text)) {
    | Ok(_) => Alcotest.failf("%s: expected an error", what)
    | Error(_) => ()
    };
  is_error("not JSON", "{");
  is_error("not an object", "[]");
  is_error("no edges", {|{"nodes":[],"values":[],"loose_tests":[]}|});
  is_error(
    "a node without a key",
    {|{"nodes":[{"label":"t"}],"edges":[],"values":[],"loose_tests":[]}|},
  );
  /* a graph written before values had [doc], [deps] and [exported] */
  let old = {|{"nodes":[],"edges":[],"loose_tests":[],"values":[{"name":"A.x","label":"x","m_path":["A"],"key":"int","ty":"int","err":false,"at":{"file":"a.ml","path":"A/x","line":1,"end_line":1}}]}|};
  switch (G.of_string(old)) {
  | Ok({values: [v], _}) =>
    Alcotest.(check(bool))("exported by default", true, v.v_exported);
    Alcotest.check(strings, "no deps", [], v.v_deps);
  | _ => Alcotest.fail("an older graph should still be read")
  };
};

let formers = () => {
  let check = (g, key, former, parts) => {
    let n = node(g, key);
    Alcotest.(check(option(string)))(key ++ " former", former, n.former);
    Alcotest.check(strings, key ++ " parts", parts, n.parts);
  };
  let vector = extract("vector");
  check(vector, "Scene.point", Some("()"), ["int", "int"]);
  check(vector, "Scene.scene", Some("[]"), ["Scene.shape"]);
  check(vector, "Scene.shape", None, []);
  let shapes = extract("shapes");
  check(shapes, "Geo.vec", Some("{}"), ["int", "int"]);
  check(shapes, "Geo.length", Some("="), ["int"]); /* hidden, but so it is inside */
  check(shapes, "Geo.Angle.t", None, []);
  let plane = extract("plane");
  check(plane, "Point.t", Some("{}"), ["int", "int"]);
  check(plane, "Point.pair", Some("()"), ["Point.t", "Point.t"]);
  check(plane, "Path.t", Some("[]"), ["Point.t"]);
  check(plane, "Path.ends", Some("?"), ["Point.pair"]);
  List.iter(name => check_well_formed(name, extract(name)), examples);
  /* in the JSON: a string, or no field at all */
  let field = (g, key) =>
    switch (G.to_json(g)) {
    | `Assoc(top) =>
      switch (List.assoc("nodes", top)) {
      | `List(nodes) =>
        List.find_map(
          fun
          | `Assoc(n) when List.assoc("key", n) == `String(key) =>
            Some(List.assoc_opt("former", n))
          | _ => None,
          nodes,
        )
        |> Option.join
      | _ => None
      }
    | _ => None
    };
  Alcotest.(check(bool))(
    "point has \"former\": \"()\"",
    true,
    field(vector, "Scene.point") == Some(`String("()")),
  );
  Alcotest.(check(bool))(
    "shape has no \"former\"",
    true,
    field(vector, "Scene.shape") == None,
  );
};

let values = () => {
  let plane = extract("plane");
  let origin = value(plane, "Point.origin");
  Alcotest.(check(option(string)))(
    "doc, from the interface",
    Some("Where the axes cross."),
    origin.v_doc,
  );
  Alcotest.(check(bool))("origin is exported", true, origin.v_exported);
  Alcotest.(check(bool))(
    "factor is private",
    false,
    value(plane, "Point.factor").v_exported,
  );
  Alcotest.check(
    strings,
    "start mentions origin",
    ["Point.origin"],
    value(plane, "Path.start").v_deps,
  );
  Alcotest.check(strings, "origin mentions nothing", [], origin.v_deps);
  Alcotest.(check(bool))(
    "no interface: exported",
    true,
    value(extract("shapes"), "Figure.unit_dot").v_exported,
  );
  switch (G.value_to_json(origin)) {
  | `Assoc(fields) =>
    Alcotest.(check(bool))(
      "in the JSON",
      true,
      List.assoc_opt("exported", fields) == Some(`Bool(true))
      && List.assoc_opt("doc", fields)
      == Some(`String("Where the axes cross."))
      && List.assoc_opt("deps", fields) == Some(`List([])),
    )
  | _ => Alcotest.fail("a value is an object")
  };
};

/* ---------- Locate ---------- */

let sources = [
  ("vector", [("lib/scene.ml", ["Scene"])]),
  ("shapes", [("lib/figure.ml", ["Figure"]), ("lib/geo.ml", ["Geo"])]),
  ("plane", [("lib/path.re", ["Path"]), ("lib/point.re", ["Point"])]),
  (
    "checks",
    [
      ("lib/tally.ml", ["Tally"]),
      ("lib/window.re", ["Window"]),
      ("test/test_tally.ml", ["Test_tally"]),
    ],
  ),
];

/* What a scan of the text cannot find by name: what a ppx wrote, tests
   without a description, Alcotest cases written in place. */
let not_in_the_text = [
  "Tally/equal_entry",
  "Tally/equal",
  "Tally/<<(total empty) = 0>>",
  "Tally/<<(count \"a\" (add \"a\" empty)) = 1>>",
  "Test_tally/tally: total",
  "Test_tally/inside",
];

let read_file = (path: string): string =>
  In_channel.with_open_bin(path, In_channel.input_all);

/* Every handle of the graph, as Locate would name it. */
let handles = (g: G.t): list((list(Locate.kind), G.handle)) =>
  List.filter_map(
    (n: G.node) => Option.map(h => ([Locate.Type], h), n.at),
    g.nodes,
  )
  @ List.map((e: G.edge) => ([Locate.Val], e.e_at), g.edges)
  @ List.map((v: G.value) => ([Locate.Val], v.v_at), g.values)
  @ List.map(
      (t: G.test) => ([Locate.Test, Locate.Val], t.t_at),
      List.concat_map((e: G.edge) => e.tests, g.edges) @ g.loose_tests,
    );

let check_located =
    (~except=[], name: string, file: string, located: Locate.t) => {
  let found =
    List.filter(
      ((_, h: G.handle)) => h.file == file && !List.mem(h.path, except),
      handles(extract(name)),
    );
  Alcotest.(check(bool))(file ++ " has definitions", true, found != []);
  List.iter(
    ((kinds, h: G.handle)) =>
      Alcotest.(check(option(pair(int, int))))(
        Printf.sprintf("%s: %s", file, h.path),
        Some((h.line, h.end_line)),
        List.find_map(kind => Locate.find(located, kind, h.path), kinds),
      ),
    found,
  );
};

let each_source = f =>
  List.iter(
    ((name, files)) =>
      List.iter(((file, prefix)) => f(name, file, prefix), files),
    sources,
  );

let locate_exact = () =>
  each_source((name, file, prefix) => {
    let root = example(name);
    let build =
      List.fold_left(Filename.concat, root, [build_dir, "default"]);
    switch (Locate.source(~root, ~build, ~prefix, file)) {
    | Some(located) =>
      Alcotest.(check(bool))(file ++ " parses", true, located.exact);
      check_located(name, file, located);
    | None => Alcotest.failf("%s was not read", file)
    };
  });

let locate_scan = () => {
  each_source((name, file, prefix) => {
    let text = read_file(Filename.concat(example(name), file));
    let located = Locate.scan(~prefix, text);
    Alcotest.(check(bool))("a scan is approximate", false, located.exact);
    check_located(~except=not_in_the_text, name, file, located);
  });
  /* tests without a description, by their place */
  let tally =
    Locate.scan(
      ~prefix=["Tally"],
      read_file(Filename.concat(example("checks"), "lib/tally.ml")),
    );
  List.iter(
    ((path, span)) =>
      Alcotest.(check(option(pair(int, int))))(
        path,
        span,
        Locate.find(tally, Locate.Test, path),
      ),
    [
      ("Tally/#1", Some((31, 31))),
      ("Tally/#2", Some((52, 52))),
      ("Tally/#3", None),
    ],
  );
  /* what the examples do not have */
  let located =
    Locate.scan(
      ~prefix=["M"],
      String.concat(
        "\n",
        [
          "type 'a tree =",
          "  | Leaf",
          "  | Node of 'a forest",
          "",
          "and 'a forest = 'a tree list",
          "",
          "let rec even n = n = 0 || odd (n - 1)",
          "and odd n = n <> 0 && even (n - 1)",
          "",
          "let%test \"zero is even\" = even 0",
          "",
          "module Sub = struct",
          "  let x =",
          "    let y = 1 in",
          "    y",
          "end",
          "",
          "let last = 1",
        ],
      ),
    );
  let check = (kind, path, span) =>
    Alcotest.(check(option(pair(int, int))))(
      path,
      span,
      Locate.find(located, kind, path),
    );
  check(Locate.Type, "M/tree", Some((1, 3)));
  check(Locate.Type, "M/forest", Some((5, 5)));
  check(Locate.Val, "M/even", Some((7, 7)));
  check(Locate.Val, "M/odd", Some((8, 8)));
  check(Locate.Test, "M/zero is even", Some((10, 10)));
  check(Locate.Val, "M/Sub/x", Some((13, 15)));
  check(Locate.Val, "M/Sub/y", None); /* local */
  check(Locate.Val, "M/y", None);
  check(Locate.Val, "M/last", Some((18, 18)));
  Alcotest.(check(bool))(
    "line 14 is inside Sub.x",
    true,
    Locate.containing(located, 14) == [(Locate.Val, "M/Sub/x")],
  );
};

/* ---------- a broken build ---------- */

/* Copies of the examples, under a temporary directory removed on exit. */
let scratch: Lazy.t(string) =
  lazy({
    let dir = Filename.temp_dir("portolan-test-", "");
    at_exit(() => ignore(Sys.command("rm -rf " ++ Filename.quote(dir))));
    dir;
  });

let write_file = (path: string, text: string) =>
  Out_channel.with_open_bin(path, oc => Out_channel.output_string(oc, text));

/* Runs dune in [dir]: its exit code and what it printed. */
let build = (dir: string): (int, string) => {
  let log = Filename.concat(dir, "build.log");
  let code =
    Sys.command(
      Printf.sprintf(
        "cd %s && dune build --root . --build-dir %s @check > build.log 2>&1",
        Filename.quote(dir),
        build_dir,
      ),
    );
  (code, read_file(log));
};

/* A copy of an example's sources, built. */
let copy = (name: string, ~of_: string): string => {
  let dir = Filename.concat(Lazy.force(scratch), name);
  let lib = Filename.concat(dir, "lib");
  Sys.mkdir(dir, 0o755);
  Sys.mkdir(lib, 0o755);
  let from = example(of_);
  let cp = file =>
    write_file(
      Filename.concat(dir, file),
      read_file(Filename.concat(from, file)),
    );
  cp("dune-project");
  Array.iter(
    file => cp(Filename.concat("lib", file)),
    Sys.readdir(Filename.concat(from, "lib")),
  );
  if (Sys.file_exists(Filename.concat(from, "test"))) {
    Sys.mkdir(Filename.concat(dir, "test"), 0o755);
    Array.iter(
      file =>
        if (!
              Sys.is_directory(
                List.fold_left(Filename.concat, from, ["test", file]),
              )) {
          cp(Filename.concat("test", file));
        },
      Sys.readdir(Filename.concat(from, "test")),
    );
  };
  switch (build(dir)) {
  | (0, _) => dir
  | (_, log) => Alcotest.failf("%s: the copy does not build:\n%s", name, log)
  };
};

let find_sub = (~sub: string, s: string): option(int) => {
  let n = String.length(sub)
  and m = String.length(s);
  let rec at = i =>
    if (i + n > m) {
      None;
    } else if (String.sub(s, i, n) == sub) {
      Some(i);
    } else {
      at(i + 1);
    };
  at(0);
};

/* [sub] must be there. */
let rec replace = (~all=false, ~sub: string, ~by: string, s: string): string =>
  switch (find_sub(~sub, s)) {
  | None => Alcotest.failf("the source has no %S", sub)
  | Some(i) =>
    let rest =
      String.sub(
        s,
        i + String.length(sub),
        String.length(s) - i - String.length(sub),
      );
    String.sub(s, 0, i)
    ++ by
    ++ (
      if (all && find_sub(~sub, rest) != None) {
        replace(~all, ~sub, ~by, rest);
      } else {
        rest;
      }
    );
  };

let edit = (dir: string, file: string, f: string => string) => {
  let path = Filename.concat(dir, file);
  write_file(path, f(read_file(path)));
};

/* The 1-based line [sub] is on. */
let line_of = (dir: string, file: string, sub: string): int => {
  let text = read_file(Filename.concat(dir, file));
  switch (find_sub(~sub, text)) {
  | None => Alcotest.failf("%s has no %S", file, sub)
  | Some(i) =>
    1 + List.length(String.split_on_char('\n', String.sub(text, 0, i))) - 1
  };
};

/* The build must fail. The error locations, as the compiler printed
   them: File "lib/figure.ml", line 11, characters ... */
let broken_build = (dir: string): list((string, int)) =>
  switch (build(dir)) {
  | (0, _) => Alcotest.fail("the build should have failed")
  | (_, log) =>
    List.filter_map(
      line =>
        try(
          Scanf.sscanf(String.trim(line), "File %S, line %d", (file, n) =>
            Some((file, n))
          )
        ) {
        | _ => None
        },
      String.split_on_char('\n', log),
    )
  };

let extract_dir = (~failing=?, ~previous=?, ~broken=[], dir: string): G.t =>
  switch (
    Extract.extract(~root=dir, ~build_dir, ~failing, ~previous, ~broken)
  ) {
  | Ok(g) => g
  | Error(message) => Alcotest.failf("extract %s: %s", dir, message)
  };

/* Which way Locate finds the items of a file as it stands: from a parse
   tree ([exact]) or by scanning the text. */
let check_route =
    (what: string, ~exact: bool, dir: string, file: string, prefix) => {
  let build = List.fold_left(Filename.concat, dir, [build_dir, "default"]);
  switch (Locate.source(~root=dir, ~build, ~prefix, file)) {
  | Some(located) => Alcotest.(check(bool))(what, exact, located.exact)
  | None => Alcotest.failf("%s was not read", file)
  };
};

/* As the tool would get it: through the JSON. */
let saved = (g: G.t): G.t =>
  switch (G.of_string(G.to_string(g))) {
  | Ok(g) => g
  | Error(message) => Alcotest.fail(message)
  };

/* The keys and names marked as in error. */
let marked = (g: G.t): list(string) =>
  List.filter_map((n: G.node) => n.n_err ? Some(n.key) : None, g.nodes)
  @ List.filter_map(
      (e: G.edge) => e.e_err ? Some(e.e_name) : None,
      g.edges,
    )
  @ List.filter_map(
      (v: G.value) => v.v_err ? Some(v.v_name) : None,
      g.values,
    );

/* What a carried file should look like: [g] with the handles of [file]
   moved down [by] lines, its tests unknown, and [errors] marked. */
let expect = (~file: string, ~by: int, ~errors: list(string), g: G.t): G.t => {
  let move = (h: G.handle) =>
    if (h.file == file) {
      {
        ...h,
        line: h.line + by,
        end_line: h.end_line + by,
      };
    } else {
      h;
    };
  let test = (t: G.test) => {
    ...t,
    t_status:
      if (t.t_at.file == file) {
        G.Unknown;
      } else {
        t.t_status;
      },
    t_at: move(t.t_at),
  };
  {
    nodes:
      List.map(
        (n: G.node) =>
          {
            ...n,
            n_err: List.mem(n.key, errors),
            at: Option.map(move, n.at),
          },
        g.nodes,
      ),
    edges:
      List.map(
        (e: G.edge) =>
          {
            ...e,
            e_err: List.mem(e.e_name, errors),
            tests: List.map(test, e.tests),
            e_at: move(e.e_at),
          },
        g.edges,
      ),
    values:
      List.map(
        (v: G.value) =>
          {
            ...v,
            v_err: List.mem(v.v_name, errors),
            v_at: move(v.v_at),
          },
        g.values,
      ),
    loose_tests: List.map(test, g.loose_tests),
  };
};

/* Compares the two as JSON, and shows where they first differ. */
let same_graph = (what: string, expected: G.t, actual: G.t) => {
  let lines = g =>
    String.split_on_char('\n', Yojson.Safe.pretty_to_string(G.to_json(g)));
  let rec go = (i, context, e, a) =>
    switch (e, a) {
    | ([], []) => ()
    | ([x, ...e], [y, ...a]) when x == y =>
      go(i + 1, [x, ...context], e, a)
    | _ =>
      let first = l =>
        String.concat("\n", List.filteri((i, _) => i < 6, l));
      Alcotest.failf(
        "%s: the graphs differ at line %d, after\n%s\n--- expected\n%s\n--- got\n%s",
        what,
        i,
        String.concat(
          "\n",
          List.rev(List.filteri((i, _) => i < 12, context)),
        ),
        first(e),
        first(a),
      );
    };
  go(1, [], lines(expected), lines(actual));
};

/* Figure depends on Geo. A type error in Figure, while Geo gains a
   function: Geo is read, Figure is carried. */
let carried_dependent = () => {
  let dir = copy("dependent", ~of_="shapes");
  let clean = extract_dir(~failing=[], dir);
  Alcotest.(check(bool))(
    "tests pass before",
    true,
    List.for_all(
      (t: G.test) => t.t_status == G.Pass,
      edge(clean, "Figure.shift").tests,
    ),
  );
  edit(dir, "lib/figure.ml", text =>
    "(* two lines\n   above *)\n"
    ++ replace(~sub="Dot (Geo.add p d)", ~by="Dot (Geo.add p \"d\")", text)
  );
  edit(dir, "lib/geo.ml", text =>
    text ++ "\nlet twice (v : vec) : vec = add v v\n"
  );
  edit(dir, "lib/geo.mli", text => text ++ "\nval twice : vec -> vec\n");
  let broken = broken_build(dir);
  let error_line = line_of(dir, "lib/figure.ml", "Dot (Geo.add p \"d\")");
  Alcotest.(check(list(pair(string, int))))(
    "the compiler's error",
    [("lib/figure.ml", error_line)],
    broken,
  );
  /* as before: without the earlier graph, Figure is gone */
  let without = extract_dir(dir);
  Alcotest.check(
    strings,
    "no Figure",
    [],
    in_file("lib/figure.ml", without),
  );
  Alcotest.check(strings, "nothing marked", [], marked(without));

  let g = extract_dir(~failing=[], ~previous=saved(clean), ~broken, dir);
  check_well_formed("carried", g);
  /* Geo is read afresh */
  Alcotest.(check(bool))(
    "Geo.twice is there",
    true,
    has_edge(g, "Geo.twice"),
  );
  Alcotest.(check(int))(
    "where it is now",
    line_of(dir, "lib/geo.ml", "let twice"),
    edge(g, "Geo.twice").e_at.line,
  );
  Alcotest.check(
    strings,
    "Geo's other edges as in a fresh graph",
    in_file("lib/geo.ml", without),
    in_file("lib/geo.ml", g),
  );
  /* Figure is as it was: two lines down, its tests unknown, shift marked */
  Alcotest.check(
    strings,
    "only shift is marked",
    ["Figure.shift"],
    marked(g),
  );
  let expected =
    expect(~file="lib/figure.ml", ~by=2, ~errors=["Figure.shift"], clean);
  List.iter(
    (e: G.edge) =>
      if (e.e_at.file == "lib/figure.ml") {
        Alcotest.(check(bool))(
          e.e_name ++ " is carried",
          true,
          List.mem(e, g.edges),
        );
      },
    expected.edges,
  );
  Alcotest.(check(bool))(
    "the value is carried",
    true,
    List.mem(value(expected, "Figure.unit_dot"), g.values),
  );
  Alcotest.(check(bool))(
    "the type is carried",
    true,
    List.mem(node(expected, "Figure.t"), g.nodes),
  );
  Alcotest.check(
    strings,
    "tests stay on their edges",
    ["shift_dot"],
    test_names(edge(g, "Figure.shift")),
  );
  Alcotest.(check(bool))(
    "and are unknown",
    true,
    List.for_all(
      (t: G.test) => t.t_status == G.Unknown,
      edge(g, "Figure.shift").tests @ edge(g, "Figure.length").tests,
    ),
  );
  /* every node of the clean graph is still there, once */
  List.iter(
    (n: G.node) =>
      Alcotest.(check(bool))(
        n.key ++ " is there",
        true,
        has_node(g, n.key),
      ),
    clean.nodes,
  );
  /* a use from a carried definition still counts */
  Alcotest.(check(bool))("Geo.mm is main", true, edge(g, "Geo.mm").main);
};

/* A type error in Geo's implementation. Its interface still compiles,
   so Figure is read afresh, and refers to Geo's carried types. */
let dependency_broken: Lazy.t((string, G.t, list((string, int)))) =
  lazy({
    let dir = copy("dependency", ~of_="shapes");
    let clean = extract_dir(dir);
    edit(dir, "lib/geo.ml", replace(~sub=": int = l", ~by=": int = \"l\""));
    (dir, clean, broken_build(dir));
  });

let carried_dependency = () => {
  let (dir, clean, broken) = Lazy.force(dependency_broken);
  Alcotest.(check(list(pair(string, int))))(
    "the compiler's error",
    [("lib/geo.ml", 7)],
    broken,
  );
  let without = extract_dir(dir);
  Alcotest.(check(bool))(
    "without the earlier graph, Geo's types are outsiders",
    true,
    !has_node(without, "Geo.vec") && !has_edge(without, "Geo.add"),
  );
  let g = extract_dir(~previous=saved(clean), ~broken, dir);
  check_well_formed("carried", g);
  Alcotest.check(strings, "only to_mm is marked", ["Geo.to_mm"], marked(g));
  Alcotest.(check(string))(
    "a fresh edge ends on a carried type",
    "Geo.length",
    edge(g, "Figure.length").dst,
  );
  Alcotest.check(
    kind,
    "which is still the declared type",
    G.Alias,
    node(g, "Geo.length").kind,
  );
  Alcotest.check(
    strings,
    "a fresh edge's deps reach into the carried module",
    ["Geo.mm", "Geo.norm1", "Geo.Angle.to_deg"],
    edge(g, "Figure.length").e_deps,
  );
  /* nothing moved, so apart from the mark it is the clean graph, in the
     same order */
  same_graph(
    "the clean graph, marked",
    expect(~file="lib/geo.ml", ~by=0, ~errors=["Geo.to_mm"], clean),
    g,
  );
  /* and it can be carried again from its own output */
  same_graph("twice", g, extract_dir(~previous=saved(g), ~broken, dir));
};

/* What the source no longer has is dropped; what the earlier graph did
   not have is not added. */
let carried_dropped = () => {
  let dir = copy("dropped", ~of_="shapes");
  let clean = extract_dir(dir);
  /* Geo.norm1 becomes Geo.norm: Figure.length no longer compiles */
  edit(dir, "lib/geo.ml", replace(~sub="let norm1", ~by="let norm"));
  edit(dir, "lib/geo.mli", replace(~sub="val norm1", ~by="val norm"));
  edit(dir, "lib/figure.ml", text =>
    replace(
      ~sub=
        "let describe ?(unit_name = \"mm\") (f : t) : string =\n  string_of_int (Geo.to_mm (length f)) ^ unit_name\n",
      ~by="let brand_new (f : t) : t = f\n",
      text,
    )
  );
  let broken = broken_build(dir);
  let g = extract_dir(~previous=saved(clean), ~broken, dir);
  check_well_formed("carried", g);
  Alcotest.check(strings, "length is marked", ["Figure.length"], marked(g));
  Alcotest.check(
    strings,
    "describe is gone, brand_new is not added",
    ["Figure.length", "Figure.shift", "Figure.rotate", "Figure.outline"],
    in_file("lib/figure.ml", g),
  );
  Alcotest.(check(bool))(
    "the nodes only describe used are gone",
    false,
    has_node(g, "string") || has_node(g, "(string, Figure.t)"),
  );
  Alcotest.(check(bool))(
    "Geo is read afresh",
    true,
    has_edge(g, "Geo.norm") && !has_edge(g, "Geo.norm1"),
  );
  Alcotest.check(
    strings,
    "deps, less the name that is gone",
    ["Geo.mm", "Geo.Angle.to_deg"],
    edge(g, "Figure.length").e_deps,
  );
  Alcotest.(check(int))(
    "outline moved up a line",
    edge(clean, "Figure.outline").e_at.line - 1,
    edge(g, "Figure.outline").e_at.line,
  );
};

/* A carried definition refers to a type that is no longer declared. */
let carried_ghost = () => {
  let dir = copy("ghost", ~of_="shapes");
  let clean = extract_dir(dir);
  edit(dir, "lib/geo.ml", text =>
    text
    |> replace(~sub="type t = Deg", ~by="type angle = Deg")
    |> replace(~all=true, ~sub="(a : t)", ~by="(a : angle)")
    |> replace(~sub=": t = Deg", ~by=": angle = Deg")
  );
  edit(dir, "lib/geo.mli", text =>
    text
    |> replace(~sub="type t = Deg", ~by="type angle = Deg")
    |> replace(~sub="to_deg : t ->", ~by="to_deg : angle ->")
    |> replace(~sub="flip : t -> t", ~by="flip : angle -> angle")
  );
  let broken = broken_build(dir);
  let g = extract_dir(~previous=saved(clean), ~broken, dir);
  check_well_formed("carried", g);
  Alcotest.check(
    kind,
    "the new name is declared",
    G.Alias,
    node(g, "Geo.Angle.angle").kind,
  );
  Alcotest.check(
    kind,
    "the old one is a ghost",
    G.Ghost,
    node(g, "Geo.Angle.t").kind,
  );
  Alcotest.(check(bool))(
    "which rotate's argument still has as a part",
    true,
    List.mem("Geo.Angle.t", node(g, edge(g, "Figure.rotate").src).parts),
  );
  /* the error is in Figure.t's declaration: a node is marked */
  Alcotest.check(strings, "Figure.t is marked", ["Figure.t"], marked(g));
};

/* A syntax error: dune leaves the old annotations in place, and the
   file cannot be parsed either. */
let carried_unparsed = () => {
  let dir = copy("unparsed", ~of_="shapes");
  let clean = extract_dir(dir);
  edit(dir, "lib/figure.ml", text =>
    "(* two lines\n   above *)\n"
    ++ replace(
         ~sub="| Dot _ | Segment _ -> f",
         ~by="| Dot _ | Segment _ -> (f",
         text,
       )
  );
  Alcotest.(check(bool))(
    "the build reports it",
    true,
    List.exists(((file, _)) => file == "lib/figure.ml", broken_build(dir)),
  );
  let broken = [("lib/figure.ml", line_of(dir, "lib/figure.ml", "-> (f"))];
  check_route(
    "the source does not parse",
    ~exact=false,
    dir,
    "lib/figure.ml",
    ["Figure"],
  );
  /* as before: the old annotations are read, with the old lines */
  let without = extract_dir(dir);
  Alcotest.(check(int))(
    "stale lines without the earlier graph",
    edge(clean, "Figure.rotate").e_at.line,
    edge(without, "Figure.rotate").e_at.line,
  );
  let g = extract_dir(~previous=saved(clean), ~broken, dir);
  check_well_formed("carried", g);
  same_graph(
    "moved by the text scan",
    expect(~file="lib/figure.ml", ~by=2, ~errors=["Figure.rotate"], clean),
    g,
  );
};

/* An error in geo.mli: neither Geo nor Figure is compiled again. Their
   annotations still match their sources, but Geo's interface is gone, and
   with it what Geo exports: Geo is carried. The [val] the error is in
   names the definition to mark. */
let broken_interface = () => {
  let dir = copy("interface", ~of_="shapes");
  let clean = extract_dir(dir);
  edit(
    dir,
    "lib/geo.mli",
    replace(
      ~sub="val add : vec -> vec -> vec",
      ~by="val add : vec -> vecc -> vec",
    ),
  );
  let broken = broken_build(dir);
  Alcotest.(check(list(pair(string, int))))(
    "the compiler's error",
    [("lib/geo.mli", line_of(dir, "lib/geo.mli", "val add"))],
    broken,
  );
  let expected = expect(~file="", ~by=0, ~errors=["Geo.add"], clean);
  same_graph(
    "with the earlier graph",
    expected,
    extract_dir(~previous=saved(clean), ~broken, dir),
  );
  /* as before: Geo is read without its interface. It is marked all the same. */
  let without = extract_dir(~broken, dir);
  Alcotest.check(strings, "without: marked", ["Geo.add"], marked(without));
  Alcotest.(check(bool))(
    "without: everything looks exported",
    true,
    edge(without, "Geo.clamp").e_exported,
  );
};

/* The only module does not type-check. Its inline tests are found again
   in the tree ppx left. */
let carried_all = () => {
  let dir = copy("all", ~of_="vector");
  let clean = extract_dir(~failing=[], dir);
  edit(dir, "lib/scene.ml", text =>
    "(* a line above *)\n"
    ++ replace(
         ~sub="Circle (grow c, k * r)",
         ~by="Circle (grow c, \"r\")",
         text,
       )
  );
  let broken = broken_build(dir);
  switch (
    Extract.extract(
      ~root=dir,
      ~build_dir,
      ~failing=None,
      ~previous=None,
      ~broken,
    )
  ) {
  | Ok(_) => Alcotest.fail("nothing can be read: an error, as before")
  | Error(_) => ()
  };
  let g = extract_dir(~failing=[], ~previous=saved(clean), ~broken, dir);
  check_well_formed("carried", g);
  Alcotest.(check(int))(
    "an inline test, a line down",
    21,
    List.hd(edge(g, "Scene.area").tests).t_at.line,
  );
  same_graph(
    "everything carried",
    expect(~file="lib/scene.ml", ~by=1, ~errors=["Scene.scale"], clean),
    g,
  );
};

/* Reason. Point has an interface, so Path is read afresh. */
let reason_edit = (~by: string, text: string): string =>
  "/* three */\n/* lines */\n/* above */\n"
  ++ replace(
       ~sub="let double = (p: t): t => {\n  x: p.x * factor,",
       ~by,
       text,
     );

let carried_reason = () => {
  let dir = copy("reason", ~of_="plane");
  let clean = extract_dir(dir);
  edit(
    dir,
    "lib/point.re",
    reason_edit(~by="let double = (p: t): t => {\n  x: p.x ++ factor,"),
  );
  let broken = broken_build(dir);
  Alcotest.(check(list(pair(string, int))))(
    "the compiler's error",
    [("lib/point.re", line_of(dir, "lib/point.re", "p.x ++ factor"))],
    broken,
  );
  check_route(
    "the source parses: dune's tree is there",
    ~exact=true,
    dir,
    "lib/point.re",
    ["Point"],
  );
  let g = extract_dir(~previous=saved(clean), ~broken, dir);
  check_well_formed("carried", g);
  Alcotest.check(
    kind,
    "Path.t is a list of the carried Point.t",
    G.Alias,
    node(g, List.hd(node(g, "Path.t").parts)).kind,
  );
  same_graph(
    "moved, from dune's parse tree",
    expect(~file="lib/point.re", ~by=3, ~errors=["Point.double"], clean),
    g,
  );
};

let carried_reason_unparsed = () => {
  let dir = copy("reason-unparsed", ~of_="plane");
  let clean = extract_dir(dir);
  edit(
    dir,
    "lib/point.re",
    reason_edit(~by="let double = (p: t): t => {{\n  x: p.x * factor,"),
  );
  ignore(broken_build(dir));
  let broken = [("lib/point.re", line_of(dir, "lib/point.re", "{{"))];
  check_route(
    "the source does not parse: no tree",
    ~exact=false,
    dir,
    "lib/point.re",
    ["Point"],
  );
  let g = extract_dir(~previous=saved(clean), ~broken, dir);
  check_well_formed("carried", g);
  same_graph(
    "moved, by the text scan",
    expect(~file="lib/point.re", ~by=3, ~errors=["Point.double"], clean),
    g,
  );
};

/* portolan-extract --previous FILE --broken file:line prints what the
   library returns. */
/* A type error in Tally: it and the test file that uses it are carried.
   Both parse, so everything is found again by name: what a ppx wrote in
   its output, the Alcotest cases in the text of the test file. */
let carried_checks = () => {
  let dir = copy("checks", ~of_="checks");
  let clean = extract_dir(dir);
  edit(dir, "lib/tally.ml", text =>
    "(* two lines\n   above *)\n"
    ++ replace(~sub="~default:0", ~by="~default:\"none\"", text)
  );
  edit(dir, "test/test_tally.ml", text => "(* one line above *)\n" ++ text);
  let broken = broken_build(dir);
  check_route("Tally parses", ~exact=true, dir, "lib/tally.ml", ["Tally"]);
  let g = extract_dir(~previous=saved(clean), ~broken, dir);
  check_well_formed("carried", g);
  same_graph(
    "everything is where it is now",
    clean
    |> expect(~file="lib/tally.ml", ~by=2, ~errors=["Tally.count"])
    |> expect(~file="test/test_tally.ml", ~by=1, ~errors=["Tally.count"]),
    g,
  );
};

/* Tally does not parse: its text is scanned. What the scan cannot find
   by name is found by where it was: what a ppx wrote follows its type,
   a test without a description is the n-th of its module. The module
   with a signature before its body is found too. */
let carried_checks_unparsed = () => {
  let dir = copy("checks-unparsed", ~of_="checks");
  let clean = extract_dir(dir);
  edit(dir, "lib/tally.ml", text =>
    "(* three\n   lines\n   above *)\n"
    ++ replace(
         ~sub="| None -> (name, 1) :: tally",
         ~by="| None -> (name, 1 :: tally",
         text,
       )
  );
  ignore(broken_build(dir));
  check_route(
    "Tally does not parse",
    ~exact=false,
    dir,
    "lib/tally.ml",
    ["Tally"],
  );
  let g = extract_dir(~previous=saved(clean), dir);
  check_well_formed("carried", g);
  same_graph(
    "everything is where it is now",
    expect(~file="lib/tally.ml", ~by=3, ~errors=[], clean),
    g,
  );
};

let carried_cli = () => {
  let (dir, clean, broken) = Lazy.force(dependency_broken);
  let tool = {
    let exe =
      if (Filename.is_relative(Sys.executable_name)) {
        Filename.concat(Sys.getcwd(), Sys.executable_name);
      } else {
        Sys.executable_name;
      };
    Filename.concat(
      Filename.dirname(Filename.dirname(exe)),
      "portolan-extract",
    );
  };
  let previous = Filename.concat(dir, "previous.json");
  let out = Filename.concat(dir, "graph.json");
  write_file(previous, G.to_string(clean));
  let run = args =>
    Sys.command(
      Printf.sprintf(
        "%s %s --build-dir %s %s > %s 2>/dev/null",
        Filename.quote(tool),
        Filename.quote(dir),
        build_dir,
        args,
        Filename.quote(out),
      ),
    );
  let code =
    run(
      Printf.sprintf(
        "--previous %s --broken %s",
        Filename.quote(previous),
        String.concat(
          ",",
          List.map(((f, n)) => Printf.sprintf("%s:%d", f, n), broken),
        ),
      ),
    );
  Alcotest.(check(int))("exit code", 0, code);
  Alcotest.(check(string))(
    "the same graph",
    G.to_string(extract_dir(~previous=saved(clean), ~broken, dir)),
    String.trim(read_file(out)),
  );
  write_file(previous, "{}");
  Alcotest.(check(bool))(
    "a previous file that is not a graph is refused",
    true,
    run("--previous " ++ Filename.quote(previous)) != 0,
  );
};

let () = {
  let case = (name, f) => Alcotest.test_case(name, `Quick, f);
  Alcotest.run(
    "extract",
    [
      (
        "vector",
        [
          case("nodes", vector_nodes),
          case("edges", vector_edges),
          case("tests", vector_tests),
          case("failing", vector_failing),
        ],
      ),
      (
        "shapes",
        [
          case("interface", shapes_interface),
          case("modules", shapes_modules),
          case("arguments", shapes_arguments),
          case("test functions", shapes_test_functions),
        ],
      ),
      (
        "checks",
        [
          case("Alcotest cases", checks_cases),
          case("failing cases", checks_failing),
          case("what a ppx wrote", checks_generated),
          case("unnamed types", checks_formers),
        ],
      ),
      (
        "general",
        [case("deterministic", deterministic), case("errors", errors)],
      ),
      (
        "record",
        [
          case("json round trip", json_round_trip),
          case("json errors", json_errors),
          case("formers", formers),
          case("values", values),
        ],
      ),
      (
        "locate",
        [
          case("parse trees agree with the annotations", locate_exact),
          case("the text scan agrees with the annotations", locate_scan),
        ],
      ),
      (
        "broken build",
        [
          case("a dependent module is carried", carried_dependent),
          case("a module others depend on is carried", carried_dependency),
          case("definitions that are gone", carried_dropped),
          case("a type that is gone", carried_ghost),
          case("a file that does not parse", carried_unparsed),
          case("an error in an interface", broken_interface),
          case("every module broken, inline tests", carried_all),
          case("Reason, from dune's parse tree", carried_reason),
          case("Reason, from the text", carried_reason_unparsed),
          case("a ppx and Alcotest, from parse trees", carried_checks),
          case("a ppx and Alcotest, from the text", carried_checks_unparsed),
          case("the command-line tool", carried_cli),
        ],
      ),
    ],
  );
};
