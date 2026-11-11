/* CanvasGraph — the architectural graph the canvas draws: named types are
   nodes, top-level functions are edges, values dock to their type's node,
   tests attach to the functions they mention. Multi-argument functions
   get an explicit Product node fed by formation lines from the component
   types. Builtin types (int, string, ...) duplicate per use-site as small
   satellite terminals — like ground symbols in a circuit diagram — so
   they never act as hubs.

   PORTOLAN: the record types are Hazel's (src/web/app/canvas/CanvasGraph.re)
   with the same field names, so the copied canvas files read them
   unchanged. What differs:
   - an id is a string (the definition's qualified name), not a Hazel Id.t;
   - terms are source text, not Exp.t;
   - there is no extraction here: a graph arrives as Portolan's Graph JSON
     (graph/graph.ml, `Graph.to_string`) and `of_json` reads it. */

module Id = Haz3lcore.Id;

module TestStatus = {
  type t =
    | Pass
    | Fail
    | Indet;
};

type node_kind =
  | Alias /* a named type declared in the project */
  | Builtin /* int, bool, ... */
  | Derived /* 'a list, anonymous sums/arrows */
  | Ghost /* referenced but undefined, or hole */
  | Product; /* tuple-formation node for a multi-arg input */

type tynode = {
  key: string,
  label: string,
  /* enclosing module chain, outermost first ([] = top level) */
  m_path: list(string),
  n_id: option(Id.t), /* jump anchor */
  kind: node_kind,
  ctrs: list(string), /* constructor names when alias body is a sum */
  n_ty: option(string), /* alias body pretty — a second name for matching */
  n_doc: option(string),
  n_err: bool,
  deps: list(string), /* node keys this node's body/components reference */
  /* deps that are displayed through former nodes instead of plain dep
     links (they still participate in layer ordering) */
  hidden_deps: list(string),
  parts: list(string), /* Product / folded alias: component node keys (formation lines) */
  sat: option((string, bool)), /* satellite: (anchor node key, output side) */
  /* glyph of the type former an alias body IS ("()", "[]", "+"): the
     former is folded into the alias node — one object, drawn once */
  former: option(string),
};

type test_info = {
  t_id: Id.t,
  status: option(TestStatus.t), /* None = not yet evaluated */
  t_body: string, /* the tested expression, as source text */
  /* every function (edge name) the body mentions */
  t_refs: list(string),
};

type edge = {
  e_name: string, /* path-qualified, unique (dom ids, focus keys) */
  e_label: string, /* bare display name */
  m_path: list(string),
  e_id: Id.t, /* jump anchor */
  e_ty: string, /* full pretty type, for tooltip */
  e_src: string, /* input node key (component, product, or satellite) */
  dst: string,
  e_doc: option(string),
  e_err: bool,
  e_hole: bool, /* definition contains a hole => ghost/obligation styling */
  main: bool,
  tests: list(test_info),
  /* Hazel's probe-sample anchors: always empty here */
  e_arg_ids: list(Id.t),
  e_whole_ids: list(Id.t),
  e_out_id: option(Id.t),
  /* names of OTHER top-level bindings this definition references
     (call/constant dependencies; the hover fan) */
  e_deps: list(string),
};

type value = {
  v_name: string, /* path-qualified, unique */
  v_label: string, /* bare display name */
  m_path: list(string),
  v_id: Id.t,
  v_key: string, /* node key of the value's type */
  v_ty: string,
  v_err: bool,
  v_def: string /* the definition, as source text ("" when not carried) */
};

/* PORTOLAN: where a definition lives (Graph.handle) */
type handle = {
  file: string,
  path: string,
  line: int,
  end_line: int,
};

type t = {
  nodes: list(tynode),
  edges: list(edge),
  values: list(value),
  loose_tests: list(test_info),
  /* every test in program order (subject-attached ones included) */
  all_tests: list(test_info),
  /* Hazel's authoring / livelit fields: always empty here */
  last_def: option((string, Id.t)),
  livelit_defs: list(Id.t),
  /* PORTOLAN: id -> where it is declared */
  handles: list((Id.t, handle)),
};

let empty: t = {
  nodes: [],
  edges: [],
  values: [],
  loose_tests: [],
  all_tests: [],
  last_def: None,
  livelit_defs: [],
  handles: [],
};

let mk_node =
    (
      ~n_id=None,
      ~m_path=[],
      ~ctrs=[],
      ~n_ty=None,
      ~n_doc=None,
      ~n_err=false,
      ~deps=[],
      ~hidden_deps=[],
      ~parts=[],
      ~sat=None,
      ~former=None,
      ~kind,
      ~label,
      key,
    )
    : tynode => {
  key,
  label,
  m_path,
  n_id,
  kind,
  ctrs,
  n_ty,
  n_doc,
  n_err,
  deps,
  hidden_deps,
  parts,
  sat,
  former,
};

let truncate = (n: int, s: string): string =>
  String.length(s) > n ? String.sub(s, 0, n - 1) ++ {js|…|js} : s;

let display_label = (key: string): string =>
  if (String.length(key) > 0 && key.[0] == '?') {
    "?";
  } else if (String.length(key) >= 2 && String.sub(key, 0, 2) == "{}") {
    "{}";
  } else {
    truncate(20, key);
  };

/* a repeated definition (a second `type pos`) would give two nodes one
   key, and the vdom two keyed children with one key — which breaks its
   child patching (insertBefore of nothing). Later duplicates get `#2`,
   `#3`…; references by key resolve to the first. */
let uniquify_keys = (nodes: list(tynode)): list(tynode) => {
  let seen = Hashtbl.create(16);
  List.map(
    (n: tynode) => {
      let count =
        Option.value(Hashtbl.find_opt(seen, n.key), ~default=0) + 1;
      Hashtbl.replace(seen, n.key, count);
      count == 1
        ? n
        : {
          ...n,
          key: n.key ++ "#" ++ string_of_int(count),
        };
    },
    nodes,
  );
};

let qname = (path: list(string), name: string): string =>
  String.concat(".", path @ [name]);

let is_former_key = (key: string): bool =>
  String.length(key) >= 3 && String.sub(key, 0, 3) == "{}@";
let former_key = (path: list(string)): string =>
  "{}@" ++ String.concat(".", path);

/* ---------- PORTOLAN: reading Graph JSON ---------- */

/* The canvas wants a few things Portolan's record does not say outright.
   Each is read from the JSON when the producer supplies it (optional
   fields `deps`, `hidden_deps`, `sat`, `former` on a node) and derived
   here, the way Hazel's extraction does it, when it does not:

   - satellites: a builtin node that is a function's only argument, its
     result, or a component of a product is split into one small terminal
     per use-site, docked to the node at the other end;
   - `deps` of a named type: the other named types its body mentions;
   - `former`: "()" for a type with component parts, "+" for a sum;
   - a module's former node ("{}@M"): the host its members cluster on. */
module Read = {
  module J = Yojson.Safe.Util;
  type json = Yojson.Safe.t;

  let mem = (k: string, j: json): json =>
    switch (j) {
    | `Assoc(kvs) => Option.value(List.assoc_opt(k, kvs), ~default=`Null)
    | _ => `Null
    };
  let str = (~default="", k, j): string =>
    switch (mem(k, j)) {
    | `String(s) => s
    | _ => default
    };
  let str_opt = (k, j): option(string) =>
    switch (mem(k, j)) {
    | `String(s) => Some(s)
    | _ => None
    };
  let bool = (k, j): bool =>
    switch (mem(k, j)) {
    | `Bool(b) => b
    | _ => false
    };
  let int = (k, j): int =>
    switch (mem(k, j)) {
    | `Int(i) => i
    | _ => 0
    };
  let list = (k, j): list(json) =>
    switch (mem(k, j)) {
    | `List(l) => l
    | _ => []
    };
  let strs = (k, j): list(string) =>
    List.filter_map(
      fun
      | `String(s) => Some(s)
      | _ => None,
      list(k, j),
    );
  let strs_opt = (k, j): option(list(string)) =>
    switch (mem(k, j)) {
    | `List(_) => Some(strs(k, j))
    | _ => None
    };

  let handle = (j: json): option(handle) =>
    switch (j) {
    | `Assoc(_) =>
      Some({
        file: str("file", j),
        path: str("path", j),
        line: int("line", j),
        end_line: int("end_line", j),
      })
    | _ => None
    };

  let kind = (s: string): node_kind =>
    switch (s) {
    | "alias" => Alias
    | "builtin" => Builtin
    | "derived" => Derived
    | "ghost" => Ghost
    | "product" => Product
    | _ => Ghost
    };

  let status = (s: string): option(TestStatus.t) =>
    switch (s) {
    | "pass" => Some(TestStatus.Pass)
    | "fail" => Some(TestStatus.Fail)
    | "indet" => Some(TestStatus.Indet)
    | _ => None
    };

  /* identifiers in a printed type: "point * shape list" -> point, shape,
     list; a dotted path stays whole ("Geo.point") */
  let idents = (s: string): list(string) => {
    let is_id = c =>
      switch (c) {
      | 'a' .. 'z'
      | 'A' .. 'Z'
      | '0' .. '9'
      | '_'
      | '\''
      | '.' => true
      | _ => false
      };
    let n = String.length(s);
    let rec go = (i, start, acc) =>
      if (i >= n) {
        start < i ? [String.sub(s, start, i - start), ...acc] : acc;
      } else if (is_id(s.[i])) {
        go(i + 1, start, acc);
      } else {
        go(
          i + 1,
          i + 1,
          start < i ? [String.sub(s, start, i - start), ...acc] : acc,
        );
      };
    List.rev(go(0, 0, []));
  };

  /* "shape list": the body is one type under a container */
  let ends_with_container = (ty: string): bool =>
    List.exists(
      c => {
        let lt = String.length(ty)
        and lc = String.length(c);
        lt > lc && String.sub(ty, lt - lc, lc) == c;
      },
      [" list", " array", " option", " seq"],
    );

  let test = (j: json): (test_info, option(handle)) => {
    let name = str("name", j);
    (
      {
        t_id: "test:" ++ name,
        status: status(str("status", j)),
        t_body: str(~default=name, "body", j),
        t_refs: strs("refs", j),
      },
      handle(mem("at", j)),
    );
  };

  let prefixes = (path: list(string)): list(list(string)) => {
    let rec go = (acc, rev_done, rest) =>
      switch (rest) {
      | [] => List.rev(acc)
      | [x, ...xs] =>
        let p = List.rev([x, ...rev_done]);
        go([p, ...acc], [x, ...rev_done], xs);
      };
    go([], [], path);
  };

  let graph = (j: json): t => {
    let handles: ref(list((Id.t, handle))) = ref([]);
    let note = (id: Id.t, h: option(handle)) =>
      switch (h) {
      | Some(h) => handles := [(id, h), ...handles^]
      | None => ()
      };
    /* (node, deps given?, former given?) */
    let raw_nodes =
      List.map(
        n => {
          let key = str("key", n);
          note(key, handle(mem("at", n)));
          let sat =
            switch (mem("sat", n)) {
            | `Assoc(_) as s => Some((str("anchor", s), bool("output", s)))
            | _ => None
            };
          let kind = kind(str("kind", n));
          (
            {
              key,
              /* a tuple-formation dot is drawn with its glyph; what it is
                 a tuple OF is told by its formation lines (and its key) */
              label:
                kind == Product && !is_former_key(key)
                  ? "()" : str(~default=display_label(key), "label", n),
              m_path: strs("m_path", n),
              n_id: mem("at", n) == `Null ? None : Some(key),
              kind,
              ctrs: strs("ctors", n),
              n_ty: str_opt("ty", n),
              n_doc: str_opt("doc", n),
              n_err: bool("err", n),
              deps: strs("deps", n),
              hidden_deps: strs("hidden_deps", n),
              parts: strs("parts", n),
              sat,
              former: str_opt("former", n),
            },
            strs_opt("deps", n) != None,
            mem("former", n) != `Null,
          );
        },
        list("nodes", j),
      );
    let all_tests: ref(list(test_info)) = ref([]);
    let tests_of = (l: list(json)): list(test_info) =>
      List.map(
        tj => {
          let (t, h) = test(tj);
          note(t.t_id, h);
          all_tests := [t, ...all_tests^];
          t;
        },
        l,
      );
    let edges: list(edge) =
      List.map(
        e => {
          let name = str("name", e);
          note(name, handle(mem("at", e)));
          {
            e_name: name,
            e_label: str(~default=name, "label", e),
            m_path: strs("m_path", e),
            e_id: name,
            e_ty: str("ty", e),
            e_src: str("src", e),
            dst: str("dst", e),
            e_doc: str_opt("doc", e),
            e_err: bool("err", e),
            e_hole: bool("hole", e),
            main: bool("main", e),
            tests: tests_of(list("tests", e)),
            e_arg_ids: [],
            e_whole_ids: [],
            e_out_id: None,
            e_deps: strs("deps", e),
          };
        },
        list("edges", j),
      );
    let values: list(value) =
      List.map(
        v => {
          let name = str("name", v);
          note(name, handle(mem("at", v)));
          {
            v_name: name,
            v_label: str(~default=name, "label", v),
            m_path: strs("m_path", v),
            v_id: name,
            v_key: str("key", v),
            v_ty: str("ty", v),
            v_err: bool("err", v),
            v_def: str("def", v),
          };
        },
        list("values", j),
      );
    let loose_tests = tests_of(list("loose_tests", j));

    /* ---- derive what the producer left out ---- */
    let nodes0 = List.map(((n, _, _)) => n, raw_nodes);
    let find0 = (k: string): option(tynode) =>
      List.find_opt((n: tynode) => n.key == k, nodes0);
    let producer_sats = List.exists((n: tynode) => n.sat != None, nodes0);
    let plain_builtin = (k: string): bool =>
      !producer_sats
      && (
        switch (find0(k)) {
        | Some(n) => n.kind == Builtin && n.sat == None
        | None => false
        }
      );
    let label_of = (k: string): string =>
      switch (find0(k)) {
      | Some(n) => n.label
      | None => display_label(k)
      };
    let sats: ref(list(tynode)) = ref([]);
    let ensure_sat =
        (~m_path=[], ~anchor_key: string, ~output: bool, ~dup: string, k)
        : string => {
      let label = label_of(k);
      let key = label ++ "@" ++ dup;
      if (!List.exists((n: tynode) => n.key == key, sats^)) {
        sats :=
          sats^
          @ [
            mk_node(
              ~kind=Builtin,
              ~m_path,
              ~sat=Some((anchor_key, output)),
              ~label,
              key,
            ),
          ];
      };
      key;
    };
    /* components: a builtin part becomes a terminal of its product or
       of the named type it is a component of */
    let nodes1 =
      List.map(
        ((n: tynode, deps_given, former_given)) => {
          let m_path =
            switch (n.kind) {
            | Product =>
              /* the first function that takes this tuple */
              switch (
                List.find_opt((e: edge) => e.e_src == n.key, edges)
              ) {
              | Some(e) => e.m_path
              | None => []
              }
            | _ => []
            };
          let parts =
            List.mapi(
              (i, p) =>
                plain_builtin(p)
                  ? ensure_sat(
                      ~m_path,
                      ~anchor_key=n.key,
                      ~output=false,
                      ~dup=n.key ++ "c" ++ string_of_int(i),
                      p,
                    )
                  : p,
              n.parts,
            );
          let former =
            former_given
              ? n.former
              : (
                switch (n.kind) {
                | Alias
                    when
                      List.length(parts) == 1
                      && (
                        switch (n.n_ty) {
                        | Some(ty) => ends_with_container(ty)
                        | None => false
                        }
                      ) =>
                  Some("[]")
                | Alias when parts != [] => Some("()")
                | Alias
                    when
                      switch (n.n_ty) {
                      | Some(ty) => String.contains(ty, '|')
                      | None => false
                      } =>
                  Some("+")
                | _ => None
                }
              );
          let deps =
            deps_given
              ? n.deps
              : (
                switch (n.kind) {
                | Product =>
                  List.filter(p => !plain_builtin(p), n.parts)
                  |> List.sort_uniq(compare)
                | Alias =>
                  /* the named types the body mentions */
                  let named = (id: string): option(string) =>
                    List.find_map(
                      ((m: tynode, _, _)) =>
                        m.kind == Alias
                        && m.key != n.key
                        && (
                          m.key == id
                          || m.key == qname(n.m_path, id)
                          || m.label == id
                          && m.m_path == n.m_path
                        )
                          ? Some(m.key) : None,
                      raw_nodes,
                    );
                  let of_body =
                    switch (n.n_ty) {
                    | Some(ty) => List.filter_map(named, idents(ty))
                    | None => []
                    };
                  let of_parts =
                    List.filter(
                      p =>
                        switch (find0(p)) {
                        | Some(m) => m.kind != Builtin && m.key != n.key
                        | None => false
                        },
                      n.parts,
                    );
                  List.sort_uniq(compare, of_body @ of_parts);
                | _ => []
                }
              );
          let hidden_deps =
            n.hidden_deps == [] && n.kind == Alias && parts != []
              ? deps : n.hidden_deps;
          {
            ...n,
            parts,
            former,
            deps,
            hidden_deps,
          };
        },
        raw_nodes,
      );
    /* functions: a single builtin argument docks at the result, a
       builtin result docks at the input */
    let edges =
      List.map(
        (e: edge) => {
          let src =
            plain_builtin(e.e_src) && !plain_builtin(e.dst)
              ? ensure_sat(
                  ~m_path=e.m_path,
                  ~anchor_key=e.dst,
                  ~output=false,
                  ~dup=e.e_name,
                  e.e_src,
                )
              : e.e_src;
          let dst =
            plain_builtin(e.dst)
              ? ensure_sat(
                  ~m_path=e.m_path,
                  ~anchor_key=src,
                  ~output=true,
                  ~dup=e.e_name,
                  e.dst,
                )
              : e.dst;
          {
            ...e,
            e_src: src,
            dst,
          };
        },
        edges,
      );
    /* a shared builtin nobody points at any more is gone */
    let used = (k: string): bool =>
      List.exists((e: edge) => e.e_src == k || e.dst == k, edges)
      || List.exists((v: value) => v.v_key == k, values)
      || List.exists((n: tynode) => List.mem(k, n.parts), nodes1);
    let nodes2 =
      List.filter((n: tynode) => !plain_builtin(n.key) || used(n.key), nodes1)
      @ sats^;
    /* module formers: one per module path in use */
    let paths =
      List.concat_map((n: tynode) => prefixes(n.m_path), nodes2)
      @ List.concat_map((e: edge) => prefixes(e.m_path), edges)
      @ List.concat_map((v: value) => prefixes(v.m_path), values)
      |> List.fold_left((acc, p) => List.mem(p, acc) ? acc : acc @ [p], []);
    let formers =
      List.filter_map(
        (path: list(string)) => {
          let key = former_key(path);
          List.exists((n: tynode) => n.key == key, nodes2)
            ? None
            : Some(
                mk_node(
                  ~kind=Product,
                  ~m_path=path,
                  ~label=List.nth(path, List.length(path) - 1),
                  key,
                ),
              );
        },
        paths,
      );
    {
      nodes: uniquify_keys(nodes2 @ formers),
      edges,
      values,
      loose_tests,
      all_tests: List.rev(all_tests^),
      last_def: None,
      livelit_defs: [],
      handles: List.rev(handles^),
    };
  };
};

let of_json = (j: Yojson.Safe.t): t => Read.graph(j);
let of_string = (s: string): t => of_json(Yojson.Safe.from_string(s));

let handle_of = (g: t, id: Id.t): option(handle) =>
  List.assoc_opt(id, g.handles);
