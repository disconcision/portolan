/* The graph record: what the extractor writes and the canvas reads, and
   its JSON.

   Named types are nodes, top-level functions are edges from their
   argument's node to their result's, values hang on the node of their
   type, tests on the functions they mention. It mirrors Hazel's
   CanvasGraph.t with the Hazel-specific fields made opaque: a definition
   is identified by a [handle] (file + span), and code is carried as
   source text. A producer builds one per step; the canvas diffs
   consecutive ones. */

/* What identifies a node among the nodes of one graph: the type's printed
   form, qualified ("Geo.point", "Geo.point * int"). Edges, values and
   parts point at nodes by key. */
type key = string;

/* What identifies a function or a value among the definitions of one
   graph: its qualified name ("Geo.scale"). Deps and a test's refs point
   at definitions by name. */
type name = string;

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
  key,
  label: string,
  m_path: list(string), /* enclosing modules, outermost first */
  kind: node_kind,
  ctors: list(string), /* constructor or field names */
  n_ty: option(string), /* the declaration's right-hand side */
  n_doc: option(string),
  n_err: bool,
  /* the component nodes, one level down: the arguments of a Product, and
     what an Alias or a Derived node is formed from (the fields of a
     record, the element of a list, ...). A component that is itself an
     unnamed type is a Derived node with parts of its own. */
  parts: list(key),
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
  t_refs: list(name), /* the functions it mentions */
  t_at: handle,
};

type edge = {
  e_name: name,
  e_label: string,
  e_m_path: list(string),
  e_ty: string, /* printed type */
  src: key, /* the argument, or a Product node */
  dst: key, /* the result */
  e_doc: option(string),
  e_err: bool,
  e_hole: bool, /* body is a hole or a TODO */
  main: bool, /* used from two or more places */
  e_deps: list(name), /* the functions and values its body mentions */
  e_exported: bool,
  tests: list(test),
  e_at: handle,
};

type value = {
  v_name: name,
  v_label: string,
  v_m_path: list(string),
  v_key: key, /* the node of its type */
  v_ty: string,
  v_doc: option(string),
  v_err: bool,
  v_deps: list(name), /* the functions and values its definition mentions */
  v_exported: bool,
  v_at: handle,
};

type t = {
  nodes: list(node),
  edges: list(edge),
  values: list(value),
  loose_tests: list(test) /* tests that mention no known function */
};

/* The graph of a project with nothing in it. */
let empty: t;

/* The JSON a graph is stored and sent as. Keep its shape stable: traces
   store it. */
let to_json: t => Yojson.Safe.t;

/* [to_json], printed on one line. */
let to_string: t => string;

/* The inverse of [to_json]. [Error] says which field of what is missing
   or of the wrong type. Every field [to_json] writes is required, except
   the ones added after the first traces were recorded: a node's [former],
   and a value's [doc], [deps] (none) and [exported] (true). Fields it
   does not know are ignored. */
let of_json: Yojson.Safe.t => result(t, string);

/* [of_json] of a text; [Error] also when the text is not JSON. */
let of_string: string => result(t, string);

/* How a kind and a status are spelled in the JSON: "alias", "pass", ... */
let kind_to_string: node_kind => string;
let status_to_string: test_status => string;

/* One value, as [to_json] writes it in the graph's "values". */
let value_to_json: value => Yojson.Safe.t;
