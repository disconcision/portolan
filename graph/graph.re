/* The graph record: the only thing the canvas reads.

   It mirrors Hazel's CanvasGraph.t, with the Hazel-specific fields made
   opaque: a definition is identified by a [handle] (file + span), and
   code is carried as source text. Producers (the extractor here, Hazel's
   statics there) build one of these per step; the canvas diffs
   consecutive snapshots. Keep the JSON shape stable: traces store it. */

/* Where a definition lives. [path] is the entry path, e.g. "Geo/scale". */
type handle = {
  file: string, /* relative to the project root */
  path: string,
  line: int, /* 1-based, first line of the item */
  end_line: int,
};

type node_kind =
  | Alias /* a named type declared in the project */
  | Builtin /* int, string, ... */
  | Derived /* a structural type with no name: a tuple, 'a list, an arrow */
  | Ghost /* named, but declared outside the project */
  | Product; /* the argument tuple of a multi-argument function */

type node = {
  key: string, /* unique: the type's printed form, qualified */
  label: string,
  m_path: list(string), /* enclosing modules, outermost first */
  kind: node_kind,
  ctors: list(string), /* constructor or field names */
  n_ty: option(string), /* the declaration's right-hand side */
  n_doc: option(string),
  n_err: bool,
  parts: list(string), /* for Product/Derived: keys of the component nodes */
  abstract: bool, /* hidden behind an interface */
  exported: bool,
  at: option(handle),
};

type test_status =
  | Pass
  | Fail
  | Unknown;

type test = {
  t_name: string,
  t_status: test_status,
  t_refs: list(string), /* edge names it mentions */
  t_at: handle,
};

type edge = {
  e_name: string, /* unique: qualified name */
  e_label: string,
  e_m_path: list(string),
  e_ty: string, /* printed type */
  src: string, /* node key: the argument, or a Product node */
  dst: string, /* node key: the result */
  e_doc: option(string),
  e_err: bool,
  e_hole: bool, /* body is a hole or a TODO */
  main: bool, /* used from two or more places */
  e_deps: list(string), /* edge and value names its body mentions */
  e_exported: bool,
  tests: list(test),
  e_at: handle,
};

type value = {
  v_name: string,
  v_label: string,
  v_m_path: list(string),
  v_key: string, /* node key of its type */
  v_ty: string,
  v_err: bool,
  v_at: handle,
};

type t = {
  nodes: list(node),
  edges: list(edge),
  values: list(value),
  loose_tests: list(test) /* tests that mention no known function */
};

let empty = {
  nodes: [],
  edges: [],
  values: [],
  loose_tests: [],
};

/* --- JSON, by hand so the shape is explicit --- */

let str = s => `String(s);
let strs = l => `List(List.map(str, l));
let opt = f =>
  fun
  | None => `Null
  | Some(x) => f(x);

let handle_to_json = h =>
  `Assoc([
    ("file", str(h.file)),
    ("path", str(h.path)),
    ("line", `Int(h.line)),
    ("end_line", `Int(h.end_line)),
  ]);

let kind_to_string =
  fun
  | Alias => "alias"
  | Builtin => "builtin"
  | Derived => "derived"
  | Ghost => "ghost"
  | Product => "product";

let status_to_string =
  fun
  | Pass => "pass"
  | Fail => "fail"
  | Unknown => "unknown";

let node_to_json = n =>
  `Assoc([
    ("key", str(n.key)),
    ("label", str(n.label)),
    ("m_path", strs(n.m_path)),
    ("kind", str(kind_to_string(n.kind))),
    ("ctors", strs(n.ctors)),
    ("ty", opt(str, n.n_ty)),
    ("doc", opt(str, n.n_doc)),
    ("err", `Bool(n.n_err)),
    ("parts", strs(n.parts)),
    ("abstract", `Bool(n.abstract)),
    ("exported", `Bool(n.exported)),
    ("at", opt(handle_to_json, n.at)),
  ]);

let test_to_json = t =>
  `Assoc([
    ("name", str(t.t_name)),
    ("status", str(status_to_string(t.t_status))),
    ("refs", strs(t.t_refs)),
    ("at", handle_to_json(t.t_at)),
  ]);

let edge_to_json = e =>
  `Assoc([
    ("name", str(e.e_name)),
    ("label", str(e.e_label)),
    ("m_path", strs(e.e_m_path)),
    ("ty", str(e.e_ty)),
    ("src", str(e.src)),
    ("dst", str(e.dst)),
    ("doc", opt(str, e.e_doc)),
    ("err", `Bool(e.e_err)),
    ("hole", `Bool(e.e_hole)),
    ("main", `Bool(e.main)),
    ("deps", strs(e.e_deps)),
    ("exported", `Bool(e.e_exported)),
    ("tests", `List(List.map(test_to_json, e.tests))),
    ("at", handle_to_json(e.e_at)),
  ]);

let value_to_json = v =>
  `Assoc([
    ("name", str(v.v_name)),
    ("label", str(v.v_label)),
    ("m_path", strs(v.v_m_path)),
    ("key", str(v.v_key)),
    ("ty", str(v.v_ty)),
    ("err", `Bool(v.v_err)),
    ("at", handle_to_json(v.v_at)),
  ]);

let to_json = (g: t): Yojson.Safe.t =>
  `Assoc([
    ("nodes", `List(List.map(node_to_json, g.nodes))),
    ("edges", `List(List.map(edge_to_json, g.edges))),
    ("values", `List(List.map(value_to_json, g.values))),
    ("loose_tests", `List(List.map(test_to_json, g.loose_tests))),
  ]);

let to_string = g => Yojson.Safe.to_string(to_json(g));
