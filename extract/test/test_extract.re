/* Checks the graphs extracted from the two example projects.

   Builds nothing: the examples' annotations must already be there
   (extract/test/run.sh builds them, then runs this). */

module G = Portolan_graph.Graph;

let build_dir = "_build_portolan";

/* The repository root: $PORTOLAN_ROOT, or the nearest directory above
   the working directory that holds the built examples. Under [dune test]
   the working directory is _build/default/extract/test. */
let repo_root: string = (
  {
    let built = dir =>
      Sys.file_exists(
        List.fold_left(
          Filename.concat,
          dir,
          ["examples", "vector", build_dir],
        ),
      );

    let rec up = dir =>
      if (built(dir)) {
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
      | Some(dir) when built(dir) => Some(dir)
      | _ => None
      };

    switch (from_env, up(Sys.getcwd())) {
    | (Some(dir), _)
    | (None, Some(dir)) => dir
    | (None, None) =>
      prerr_endline(
        "test_extract: the examples are not built. Run extract/test/run.sh.",
      );
      exit(1);
    };
  }: string
);

let example = name =>
  List.fold_left(Filename.concat, repo_root, ["examples", name]);

let extract = (~failing=?, name): G.t =>
  switch (
    Portolan_extract.Extract.extract(
      ~root=example(name),
      ~build_dir,
      ~failing,
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
    ["Scene.point"],
    dst.parts,
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
    ["vector", "shapes"],
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
  Alcotest.check(
    strings,
    "t list mentions t",
    ["Figure.t"],
    node(g, outline.src).parts,
  );
  Alcotest.check(
    strings,
    "option of a pair mentions vec",
    ["Geo.vec"],
    node(g, outline.dst).parts,
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

let errors = () =>
  switch (
    Portolan_extract.Extract.extract(
      ~root=example("vector"),
      ~build_dir="no_such_dir",
      ~failing=None,
    )
  ) {
  | Ok(_) => Alcotest.fail("expected an error")
  | Error(_) => ()
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
        "general",
        [case("deterministic", deterministic), case("errors", errors)],
      ),
    ],
  );
};
