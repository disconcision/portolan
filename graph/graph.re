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
  /* keys of the component nodes, one level down: the arguments of a
     Product, and what an Alias or a Derived node is formed from (the
     fields of a record, the element of a list, ...). A component that is
     itself an unnamed type is a Derived node with parts of its own. */
  parts: list(string),
  /* what an Alias or a Derived node is formed with: "()" tuple,
     "{}" record, "[]" list or array, "?" option, "->" function (its
     parts are the arguments, then the result), "=" another name for its
     one part. [None] with parts: a type applied to arguments. */
  former: option(string),
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
  v_doc: option(string),
  v_err: bool,
  v_deps: list(string), /* edge and value names its definition mentions */
  v_exported: bool,
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

/* named, so that the helpers below read [handle => json] and not the
   variant type the compiler would infer for each */
type json = Yojson.Safe.t;

let str = (s): json => `String(s);
let strs = (l): json => `List(List.map(str, l));
let opt = (f: 'a => json): (option('a) => json) =>
  fun
  | None => `Null
  | Some(x) => f(x);

let handle_to_json = (h): json =>
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

let node_to_json = (n): json =>
  `Assoc(
    [
      ("key", str(n.key)),
      ("label", str(n.label)),
      ("m_path", strs(n.m_path)),
      ("kind", str(kind_to_string(n.kind))),
      ("ctors", strs(n.ctors)),
      ("ty", opt(str, n.n_ty)),
      ("doc", opt(str, n.n_doc)),
      ("err", `Bool(n.n_err)),
      ("parts", strs(n.parts)),
    ]
    /* optional: absent unless the type is formed from its parts */
    @ (
      switch (n.former) {
      | Some(f) => [("former", str(f))]
      | None => []
      }
    )
    @ [
      ("abstract", `Bool(n.abstract)),
      ("exported", `Bool(n.exported)),
      ("at", opt(handle_to_json, n.at)),
    ],
  );

let test_to_json = (t): json =>
  `Assoc([
    ("name", str(t.t_name)),
    ("status", str(status_to_string(t.t_status))),
    ("refs", strs(t.t_refs)),
    ("at", handle_to_json(t.t_at)),
  ]);

let edge_to_json = (e): json =>
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

let value_to_json = (v): json =>
  `Assoc([
    ("name", str(v.v_name)),
    ("label", str(v.v_label)),
    ("m_path", strs(v.v_m_path)),
    ("key", str(v.v_key)),
    ("ty", str(v.v_ty)),
    ("doc", opt(str, v.v_doc)),
    ("err", `Bool(v.v_err)),
    ("deps", strs(v.v_deps)),
    ("exported", `Bool(v.v_exported)),
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

/* --- reading it back: the inverse of [to_json] --- */

exception Bad(string);

/* Every field [to_json] writes is required, except the ones added after
   the first traces were recorded: a node's [former], and a value's [doc],
   [deps] and [exported]. */
module Of_json = {
  let bad = (what, field) =>
    raise(Bad(Printf.sprintf("%s: bad or missing \"%s\"", what, field)));

  let fields = (what, j: json) =>
    switch (j) {
    | `Assoc(l) => l
    | _ => raise(Bad(what ++ ": expected an object"))
    };

  /* [Null] stands for an absent field. */
  let field = (what, name, j: json): json =>
    Option.value(List.assoc_opt(name, fields(what, j)), ~default=`Null);

  let string = (what, name, j) =>
    switch (field(what, name, j)) {
    | `String(s) => s
    | _ => bad(what, name)
    };
  let string_opt = (what, name, j) =>
    switch (field(what, name, j)) {
    | `String(s) => Some(s)
    | `Null => None
    | _ => bad(what, name)
    };
  let bool = (~default=?, what, name, j) =>
    switch (field(what, name, j), default) {
    | (`Bool(b), _) => b
    | (`Null, Some(b)) => b
    | _ => bad(what, name)
    };
  let int = (what, name, j) =>
    switch (field(what, name, j)) {
    | `Int(i) => i
    | _ => bad(what, name)
    };
  let list = (~optional=false, what, name, j): list(json) =>
    switch (field(what, name, j)) {
    | `List(l) => l
    | `Null when optional => []
    | _ => bad(what, name)
    };
  let strings = (~optional=?, what, name, j) =>
    List.map(
      fun
      | `String(s) => s
      | _ => bad(what, name),
      list(~optional?, what, name, j),
    );

  let handle = (j: json): handle => {
    file: string("handle", "file", j),
    path: string("handle", "path", j),
    line: int("handle", "line", j),
    end_line: int("handle", "end_line", j),
  };

  let kind =
    fun
    | "alias" => Alias
    | "builtin" => Builtin
    | "derived" => Derived
    | "ghost" => Ghost
    | "product" => Product
    | s => raise(Bad("node: unknown kind " ++ s));

  let status =
    fun
    | "pass" => Pass
    | "fail" => Fail
    | "unknown" => Unknown
    | s => raise(Bad("test: unknown status " ++ s));

  let node = (j: json): node => {
    key: string("node", "key", j),
    label: string("node", "label", j),
    m_path: strings("node", "m_path", j),
    kind: kind(string("node", "kind", j)),
    ctors: strings("node", "ctors", j),
    n_ty: string_opt("node", "ty", j),
    n_doc: string_opt("node", "doc", j),
    n_err: bool("node", "err", j),
    parts: strings("node", "parts", j),
    former: string_opt("node", "former", j),
    abstract: bool("node", "abstract", j),
    exported: bool("node", "exported", j),
    at:
      switch (field("node", "at", j)) {
      | `Null => None
      | h => Some(handle(h))
      },
  };

  let test = (j: json): test => {
    t_name: string("test", "name", j),
    t_status: status(string("test", "status", j)),
    t_refs: strings("test", "refs", j),
    t_at: handle(field("test", "at", j)),
  };

  let edge = (j: json): edge => {
    e_name: string("edge", "name", j),
    e_label: string("edge", "label", j),
    e_m_path: strings("edge", "m_path", j),
    e_ty: string("edge", "ty", j),
    src: string("edge", "src", j),
    dst: string("edge", "dst", j),
    e_doc: string_opt("edge", "doc", j),
    e_err: bool("edge", "err", j),
    e_hole: bool("edge", "hole", j),
    main: bool("edge", "main", j),
    e_deps: strings("edge", "deps", j),
    e_exported: bool("edge", "exported", j),
    tests: List.map(test, list("edge", "tests", j)),
    e_at: handle(field("edge", "at", j)),
  };

  let value = (j: json): value => {
    v_name: string("value", "name", j),
    v_label: string("value", "label", j),
    v_m_path: strings("value", "m_path", j),
    v_key: string("value", "key", j),
    v_ty: string("value", "ty", j),
    v_doc: string_opt("value", "doc", j),
    v_err: bool("value", "err", j),
    v_deps: strings(~optional=true, "value", "deps", j),
    v_exported: bool(~default=true, "value", "exported", j),
    v_at: handle(field("value", "at", j)),
  };

  let graph = (j: json): t => {
    nodes: List.map(node, list("graph", "nodes", j)),
    edges: List.map(edge, list("graph", "edges", j)),
    values: List.map(value, list("graph", "values", j)),
    loose_tests: List.map(test, list("graph", "loose_tests", j)),
  };
};

let of_json = (j: Yojson.Safe.t): result(t, string) =>
  switch (Of_json.graph(j)) {
  | g => Ok(g)
  | exception (Bad(message)) => Error(message)
  };

let of_string = (s: string): result(t, string) =>
  switch (Yojson.Safe.from_string(s)) {
  | j => of_json(j)
  | exception (Yojson.Json_error(message)) => Error(message)
  };
