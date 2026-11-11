/* What the canvas makes of a graph record: the rules of CanvasGraph.Read
   and of the labels it shortens, each on a record a few items long. */

module G = Portolan_graph.Graph;

/* ---------- small records ---------- */

let here: G.handle = {
  file: "lib/geo.ml",
  path: "Geo",
  line: 1,
  end_line: 1,
};

let node =
    (~kind=G.Alias, ~m_path=[], ~ctors=[], ~ty=?, ~parts=[], ~former=?, key)
    : G.node => {
  key,
  label: key,
  m_path,
  kind,
  ctors,
  n_ty: ty,
  n_doc: None,
  n_err: false,
  parts,
  former,
  abstract: false,
  exported: true,
  at: kind == G.Alias ? Some(here) : None,
};

let builtin = node(~kind=G.Builtin);
let unnamed = node(~kind=G.Derived);

let test = (~status=G.Pass, ~line=1, name, refs): G.test => {
  t_name: name,
  t_status: status,
  t_refs: refs,
  t_at: {
    ...here,
    line,
  },
};

let edge = (~m_path=[], ~tests=[], name, src, dst): G.edge => {
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
  tests,
  e_at: here,
};

let value = (name, key): G.value => {
  v_name: name,
  v_label: name,
  v_m_path: [],
  v_key: key,
  v_ty: key,
  v_doc: None,
  v_err: false,
  v_deps: [],
  v_exported: true,
  v_at: here,
};

let picture = (~edges=[], ~values=[], ~loose_tests=[], nodes): CanvasGraph.t =>
  CanvasGraph.of_graph({
    nodes,
    edges,
    values,
    loose_tests,
  });

/* ---------- reading a picture ---------- */

let keys = (p: CanvasGraph.t): list(string) =>
  List.map((n: CanvasGraph.tynode) => n.key, p.nodes);

let find = (p: CanvasGraph.t, key: string): CanvasGraph.tynode =>
  switch (List.find_opt((n: CanvasGraph.tynode) => n.key == key, p.nodes)) {
  | Some(n) => n
  | None => Alcotest.fail("no node " ++ key)
  };

let the_edge = (p: CanvasGraph.t): CanvasGraph.edge =>
  switch (p.edges) {
  | [e] => e
  | _ => Alcotest.fail("expected one edge")
  };

let strings = Alcotest.(list(string));
let text = Alcotest.(option(string));
let dock = Alcotest.(option(pair(string, bool)));
let yes = (what, b) => Alcotest.(check(bool))(what, true, b);

/* ---------- terminals ---------- */

let builtin_argument = () => {
  let p =
    picture(
      ~edges=[edge("origin", "int", "point")],
      [builtin("int"), node("point")],
    );
  Alcotest.(check(string))("from", "int@origin", the_edge(p).e_src);
  let terminal = find(p, "int@origin");
  Alcotest.(check(string))("label", "int", terminal.label);
  Alcotest.check(dock, "docked", Some(("point", false)), terminal.sat);
  Alcotest.check(
    strings,
    "no shared int",
    ["point", "int@origin"],
    keys(p),
  );
};

let builtin_result = () => {
  let p =
    picture(
      ~edges=[edge("area", "shape", "int"), edge("sides", "shape", "int")],
      [builtin("int"), node("shape")],
    );
  Alcotest.check(
    strings,
    "one each",
    ["shape", "int@area", "int@sides"],
    keys(p),
  );
  Alcotest.check(
    dock,
    "docked",
    Some(("shape", true)),
    find(p, "int@area").sat,
  );
};

let builtin_to_builtin = () => {
  let p =
    picture(
      ~edges=[edge("show", "int", "string")],
      [builtin("int"), builtin("string")],
    );
  let e = the_edge(p);
  Alcotest.(check(string))("from", "int", e.e_src);
  Alcotest.(check(string))("to", "string@show", e.dst);
  Alcotest.check(
    dock,
    "docked",
    Some(("int", true)),
    find(p, "string@show").sat,
  );
};

let type_variable = () => {
  let p =
    picture(
      ~edges=[edge("wrap", "'a", "box")],
      [unnamed("'a"), node("box")],
    );
  Alcotest.(check(string))("from", "'a@wrap", the_edge(p).e_src);
};

let record_fields = () => {
  let p =
    picture([
      builtin("int"),
      builtin("string"),
      node(
        ~former="{}",
        ~ctors=["x", "name"],
        ~ty="{ x : int; name : string }",
        ~parts=["int", "string"],
        "tag",
      ),
    ]);
  let tag = find(p, "tag");
  Alcotest.check(strings, "parts", ["int@tagc0", "string@tagc1"], tag.parts);
  Alcotest.check(
    dock,
    "docked",
    Some(("tag", false)),
    find(p, "int@tagc0").sat,
  );
  Alcotest.check(strings, "not a sum", [], tag.ctrs);
  Alcotest.check(text, "former", Some("{}"), tag.former);
};

/* ---------- unnamed types ---------- */

let pair_of_types = [
  node("point"),
  node("shape"),
  unnamed(~former="()", ~parts=["point", "shape"], "point * shape"),
];

let tuple_a_function_takes = () => {
  let p =
    picture(
      ~edges=[edge(~m_path=["Geo"], "Geo.place", "point * shape", "shape")],
      pair_of_types,
    );
  let dot = find(p, "point * shape");
  yes("a formation dot", dot.kind == CanvasGraph.Product);
  Alcotest.(check(string))("glyph", "()", dot.label);
  Alcotest.check(strings, "fed by", ["point", "shape"], dot.parts);
  Alcotest.check(text, "tooltip", Some("point * shape"), dot.n_doc);
  Alcotest.check(strings, "its function's module", ["Geo"], dot.m_path);
};

let list_that_holds_a_value = () => {
  let p =
    picture(
      ~values=[value("all", "shape list")],
      [
        node("shape"),
        unnamed(~former="[]", ~parts=["shape"], "shape list"),
      ],
    );
  let n = find(p, "shape list");
  yes("text, not a dot", n.kind == CanvasGraph.Derived);
  Alcotest.(check(string))("caption", "all: shape list", n.label);
  Alcotest.check(strings, "no parts drawn", [], n.parts);
  Alcotest.check(text, "tooltip", Some("shape list"), n.n_doc);
};

let long_function_type = () => {
  let whole = "(Geo.point -> Geo.shape -> bool) -> Geo.shape list -> Geo.shape list";
  let p =
    picture(
      ~edges=[edge("filter", whole, "shape")],
      [node("shape"), unnamed(~former="->", ~parts=["shape"], whole)],
    );
  let n = find(p, whole);
  yes("text, not a dot", n.kind == CanvasGraph.Derived);
  Alcotest.(check(string))(
    "short",
    {js|(…) -> shape list -> shape list|js},
    n.label,
  );
  Alcotest.check(text, "tooltip", Some(whole), n.n_doc);
  Alcotest.check(text, "former", Some({js|→|js}), n.former);
  Alcotest.check(strings, "no parts drawn", [], n.parts);
};

let argument_tuple = () => {
  let p =
    picture(
      ~edges=[edge(~m_path=["Geo"], "Geo.scale", "(point, int)", "point")],
      [
        builtin("int"),
        node("point"),
        node(~kind=G.Product, ~parts=["point", "int"], "(point, int)"),
      ],
    );
  let args = find(p, "(point, int)");
  Alcotest.(check(string))("glyph", "()", args.label);
  Alcotest.check(
    strings,
    "parts",
    ["point", "int@(point, int)c1"],
    args.parts,
  );
  Alcotest.check(strings, "its function's module", ["Geo"], args.m_path);
};

/* ---------- named types ---------- */

let body_mentions = () => {
  let p =
    picture([
      node("point"),
      node(
        ~ctors=["Circle", "Square"],
        ~ty="Circle of point * int | Square of point",
        "shape",
      ),
    ]);
  let shape = find(p, "shape");
  Alcotest.check(strings, "deps", ["point"], shape.deps);
  Alcotest.check(text, "a sum", Some("+"), shape.former);
  Alcotest.check(strings, "constructors", ["Circle", "Square"], shape.ctrs);
};

let one_line_per_part = () => {
  let p =
    picture([
      node("point"),
      node(~former="{}", ~parts=["point", "point"], "segment"),
    ]);
  let segment = find(p, "segment");
  Alcotest.check(strings, "parts", ["point"], segment.parts);
  Alcotest.check(
    strings,
    "shown by that line",
    ["point"],
    segment.hidden_deps,
  );
};

let outside_type = () => {
  let p =
    picture(
      ~edges=[edge("read", "Yojson.Safe.t", "shape")],
      [
        builtin("string"),
        node("shape"),
        node(
          ~kind=G.Ghost,
          ~former="()",
          ~parts=["string"],
          "Yojson.Safe.t",
        ),
      ],
    );
  let json = find(p, "Yojson.Safe.t");
  Alcotest.check(strings, "no parts", [], json.parts);
  Alcotest.check(text, "no former", None, json.former);
  Alcotest.check(
    strings,
    "nor their nodes",
    ["shape", "Yojson.Safe.t"],
    keys(p),
  );
};

let repeated_key = () => {
  let p = picture([node("pos"), node("pos")]);
  Alcotest.check(strings, "keys", ["pos", "pos#2"], keys(p));
};

/* ---------- what is in the picture ---------- */

let unused = () => {
  let p =
    picture([
      builtin("int"),
      node("point"),
      unnamed(~former="[]", ~parts=["int"], "int list"),
    ]);
  Alcotest.check(strings, "only the named type", ["point"], keys(p));
};

let value_caption = () => {
  let p =
    picture(
      ~values=[value("zero", "int"), value("one", "int")],
      [builtin("int")],
    );
  Alcotest.(check(string))("label", "zero, one: int", find(p, "int").label);
};

let module_formers = () => {
  let p =
    picture(
      ~edges=[
        edge(~m_path=["Geo", "Unit"], "Geo.Unit.box", "point", "shape"),
      ],
      [node(~m_path=["Geo"], "point"), node(~m_path=["Geo"], "shape")],
    );
  Alcotest.check(
    strings,
    "outermost first, after the types",
    ["point", "shape", "{}@Geo", "{}@Geo.Unit"],
    keys(p),
  );
  let inner = find(p, "{}@Geo.Unit");
  Alcotest.(check(string))("label", "Unit", inner.label);
  Alcotest.check(strings, "path", ["Geo", "Unit"], inner.m_path);
};

/* ---------- tests and handles ---------- */

let tests_and_handles = () => {
  let p =
    picture(
      ~edges=[
        edge(
          ~tests=[
            test("area of a square", ["area"]),
            test(~status=G.Unknown, ~line=7, "area of nothing", ["area"]),
          ],
          "area",
          "shape",
          "shape",
        ),
      ],
      ~loose_tests=[test(~status=G.Fail, "sanity", [])],
      [node("shape")],
    );
  let statuses =
    List.map((t: CanvasGraph.test_info) => t.status, p.all_tests);
  yes(
    "passed, not yet run, failed",
    statuses == CanvasGraph.TestStatus.[Some(Pass), None, Some(Fail)],
  );
  Alcotest.check(
    strings,
    "ids, a function's tests first",
    ["test:area of a square", "test:area of nothing", "test:sanity"],
    List.map((t: CanvasGraph.test_info) => t.t_id, p.all_tests),
  );
  let line = id =>
    Option.map(
      (h: CanvasGraph.handle) => h.line,
      CanvasGraph.handle_of(p, id),
    );
  Alcotest.(check(option(int)))(
    "a test",
    Some(7),
    line("test:area of nothing"),
  );
  Alcotest.(check(option(int)))("a function", Some(1), line("area"));
  Alcotest.(check(option(int)))("a type", Some(1), line("shape"));
  Alcotest.(check(option(int)))("nothing", None, line("perimeter"));
};

let from_json = () => {
  let g: G.t = {
    nodes: pair_of_types,
    edges: [edge("place", "point * shape", "shape")],
    values: [],
    loose_tests: [],
  };
  yes(
    "the record's JSON gives the record's picture",
    CanvasGraph.of_string(G.to_string(g)) == Ok(CanvasGraph.of_graph(g)),
  );
  yes(
    "not a record",
    Result.is_error(CanvasGraph.of_string({|{"nodes": []}|})),
  );
  yes("not JSON", Result.is_error(CanvasGraph.of_string("nodes")));
};

/* ---------- labels ---------- */

let label = CanvasGraph.Abbreviate.ty;
let same = (what, expected, ty) =>
  Alcotest.(check(string))(what, expected, label(ty));

let label_fits = () =>
  same(
    "as printed",
    "Geo.point -> Geo.shape list",
    "Geo.point -> Geo.shape list",
  );

let label_variant = () =>
  same(
    "two tags",
    {js|[> `Assoc | `Null | …]|js},
    "[> `Assoc of (string * Yojson.Safe.t) list | `Null | `Int of int ]",
  );

let label_paths = () => {
  same(
    "the last module",
    "Node.t -> Effect.t",
    "Virtual_dom.Vdom.Node.t -> Virtual_dom.Vdom.Effect.t",
  );
  same(
    "no module",
    "tynode list -> edge list -> bool",
    "CanvasGraph.tynode list -> CanvasGraph.edge list -> bool",
  );
};

let label_brackets = () =>
  same(
    "closed up",
    {js|(…) list -> (…) option|js},
    "(int * string * bool * float) list -> (string * int * bool) option",
  );

let label_cut = () => {
  let long = String.make(40, 'a');
  same("cut", String.make(31, 'a') ++ {js|…|js}, long);
};

let () = {
  let case = (name, f) => Alcotest.test_case(name, `Quick, f);
  Alcotest.run(
    "canvas graph",
    [
      (
        "terminals",
        [
          case("a builtin argument docks at the result", builtin_argument),
          case("each use of a builtin result has its own", builtin_result),
          case(
            "between two builtins, only the result is one",
            builtin_to_builtin,
          ),
          case("a type variable is one too", type_variable),
          case("a record's builtin fields dock at the record", record_fields),
        ],
      ),
      (
        "unnamed types",
        [
          case(
            "a tuple a function takes is a dot fed by its parts",
            tuple_a_function_takes,
          ),
          case(
            "a list that only holds a value keeps its text, with the value's name",
            list_that_holds_a_value,
          ),
          case(
            "a long function type is shortened, whole in the tooltip",
            long_function_type,
          ),
          case(
            "an argument tuple is a dot in its function's module",
            argument_tuple,
          ),
        ],
      ),
      (
        "named types",
        [
          case("a sum depends on the types its body mentions", body_mentions),
          case("two fields of one type draw one line", one_line_per_part),
          case("a type from outside shows no parts", outside_type),
          case("a key used twice gets a suffix", repeated_key),
        ],
      ),
      (
        "the picture",
        [
          case("what nothing uses is left out", unused),
          case("a builtin that only holds values names them", value_caption),
          case("each module in use has a former", module_formers),
          case(
            "tests keep their status and where they are written",
            tests_and_handles,
          ),
          case("JSON goes through the record", from_json),
        ],
      ),
      (
        "labels",
        [
          case("a type that fits is left as printed", label_fits),
          case("a variant keeps its first two tags", label_variant),
          case("module paths are cut, then dropped", label_paths),
          case("inner brackets close up", label_brackets),
          case("what is still too long is cut off", label_cut),
        ],
      ),
    ],
  );
};
