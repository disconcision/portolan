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
  /* PORTOLAN: visible outside its module (not drawn yet) */
  e_exported: bool,
};

type value = {
  v_name: string, /* path-qualified, unique */
  v_label: string, /* bare display name */
  m_path: list(string),
  v_id: Id.t,
  v_key: string, /* node key of the value's type */
  v_ty: string,
  v_err: bool,
  v_def: string, /* the definition, as source text ("" when not carried) */
  /* PORTOLAN: what the record says of a value beyond Hazel's (not drawn yet) */
  v_doc: option(string),
  v_deps: list(string),
  v_exported: bool,
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

/* PORTOLAN: short labels for long printed types (Hazel cuts a label at 20
   characters; OCaml's inferred types need more care than that). */
module Abbreviate = {
  let limit = 30;
  let dots = {js|…|js};

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

  /* [f] on every maximal run of identifier characters */
  let map_idents = (f: string => string, s: string): string => {
    let b = Buffer.create(String.length(s));
    let n = String.length(s);
    let rec go = (i, start) =>
      if (i >= n) {
        if (start < i) {
          Buffer.add_string(b, f(String.sub(s, start, i - start)));
        };
      } else if (is_id(s.[i])) {
        go(i + 1, start);
      } else {
        if (start < i) {
          Buffer.add_string(b, f(String.sub(s, start, i - start)));
        };
        Buffer.add_char(b, s.[i]);
        go(i + 1, i + 1);
      };
    go(0, 0);
    Buffer.contents(b);
  };

  /* "Virtual_dom.Vdom.Effect.t" -> "Effect.t" */
  let short_path = (id: string): string =>
    switch (List.rev(String.split_on_char('.', id))) {
    | [last, before, _, ..._] => before ++ "." ++ last
    | _ => id
    };

  let closes = c => c == ')' || c == ']' || c == '}';
  let opens = c => c == '(' || c == '[' || c == '{';

  /* the index of the bracket closing the one at [i] */
  let closing = (s: string, i: int): option(int) => {
    let n = String.length(s);
    let rec go = (j, depth) =>
      if (j >= n) {
        None;
      } else if (opens(s.[j])) {
        go(j + 1, depth + 1);
      } else if (closes(s.[j])) {
        depth == 1 ? Some(j) : go(j + 1, depth - 1);
      } else {
        go(j + 1, depth);
      };
    go(i, 0);
  };

  /* A polymorphic variant keeps its first two tags and drops what they
     carry: "[> `Assoc of (string * ...) list | `Null | `Int of int ]"
     -> "[> `Assoc | `Null | …]". Either syntax. */
  let variants = (s: string): string => {
    let n = String.length(s);
    let b = Buffer.create(n);
    /* the tags directly inside the bracket at [i], if it is a variant */
    let tags = (i: int, close: int): option((string, list(string))) => {
      let rec skip = j =>
        j < close && (s.[j] == ' ' || s.[j] == '|') ? skip(j + 1) : j;
      let j = i + 1;
      let (bound, j) =
        j < close && (s.[j] == '>' || s.[j] == '<')
          ? (String.make(1, s.[j]) ++ " ", j + 1) : ("", j);
      if (skip(j) < close && s.[skip(j)] == '`') {
        let rec go = (j, depth, acc) =>
          if (j >= close) {
            List.rev(acc);
          } else if (opens(s.[j])) {
            go(j + 1, depth + 1, acc);
          } else if (closes(s.[j])) {
            go(j + 1, depth - 1, acc);
          } else if (s.[j] == '`' && depth == 0) {
            let rec stop = k => k < close && is_id(s.[k]) ? stop(k + 1) : k;
            let k = stop(j + 1);
            go(k, depth, [String.sub(s, j, k - j), ...acc]);
          } else {
            go(j + 1, depth, acc);
          };
        Some((bound, go(j, 0, [])));
      } else {
        None;
      };
    };
    let rec go = i =>
      if (i < n) {
        switch (s.[i] == '[' ? closing(s, i) : None) {
        | Some(close) =>
          switch (tags(i, close)) {
          | Some((bound, tags)) =>
            let shown =
              switch (tags) {
              | [a, b, _, ..._] => [a, b, dots]
              | l => l
              };
            Buffer.add_string(
              b,
              "[" ++ bound ++ String.concat(" | ", shown) ++ "]",
            );
            go(close + 1);
          | None =>
            Buffer.add_char(b, s.[i]);
            go(i + 1);
          }
        | None =>
          Buffer.add_char(b, s.[i]);
          go(i + 1);
        };
      };
    go(0);
    Buffer.contents(b);
  };

  /* what is inside the brackets nested deeper than [keep] becomes "…" */
  let collapse = (~keep: int, s: string): string => {
    let b = Buffer.create(String.length(s));
    let depth = ref(0);
    String.iter(
      c =>
        if (opens(c)) {
          if (depth^ <= keep) {
            Buffer.add_char(b, c);
          };
          if (depth^ == keep) {
            Buffer.add_string(b, dots);
          };
          incr(depth);
        } else if (closes(c)) {
          decr(depth);
          if (depth^ <= keep) {
            Buffer.add_char(b, c);
          };
        } else if (depth^ <= keep) {
          Buffer.add_char(b, c);
        },
      s,
    );
    Buffer.contents(b);
  };

  /* characters, not bytes: "…" is three bytes */
  let length = (s: string): int => {
    let n = ref(0);
    String.iter(c => Char.code(c) land 0xC0 != 0x80 ? incr(n) : (), s);
    n^;
  };

  let cut = (s: string): string =>
    if (length(s) <= limit) {
      s;
    } else {
      /* back to the start of a character */
      let rec back = i =>
        i > 0 && Char.code(s.[i]) land 0xC0 == 0x80 ? back(i - 1) : i;
      String.sub(s, 0, back(limit - 1)) ++ dots;
    };

  /* The first of these that fits: the type as printed; with variants
     reduced to their tags; with module paths cut to their last module;
     with the inner brackets closed up; cut off. */
  let ty = (s: string): string => {
    let fits = s => length(s) <= limit;
    let steps = [
      variants,
      map_idents(short_path),
      collapse(~keep=1),
      collapse(~keep=0),
      cut,
    ];
    List.fold_left((s, step) => fits(s) ? s : step(s), s, steps);
  };
};

/* The canvas wants a few things Portolan's record does not say outright.
   Each is read from the JSON when the producer supplies it (optional
   fields `deps`, `hidden_deps`, `sat`, `former` on a node) and derived
   here, the way Hazel's extraction does it, when it does not:

   - satellites: a builtin node (or a type variable) that is a function's
     only argument, its result, or a component is split into one small
     terminal per use-site, docked to the node at the other end;
   - `deps` of a named type: the other named types its body mentions;
   - `former`: "()" for a type with component parts, "+" for a sum;
   - a module's former node ("{}@M"): the host its members cluster on.

   And a few things are drawn differently from how the record says them:

   - an unnamed tuple, list or option (a Derived node with that former)
     that functions use or types are made of is a former dot, as Hazel
     draws a tuple written inline: its glyph, fed by its components;
   - any other unnamed type keeps its text, abbreviated (see Abbreviate);
     the whole of it is in the node's tooltip;
   - a type from outside (Ghost) shows no components of its own;
   - a node that nothing in the picture uses is left out. */
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
  let bool = (~default=false, k, j): bool =>
    switch (mem(k, j)) {
    | `Bool(b) => b
    | _ => default
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
            e_exported: bool(~default=true, "exported", e),
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
            v_doc: str_opt("doc", v),
            v_deps: strs("deps", v),
            v_exported: bool(~default=true, "exported", v),
          };
        },
        list("values", j),
      );
    let loose_tests = tests_of(list("loose_tests", j));

    /* ---- what the picture is made of ---- */
    let raw_nodes =
      List.map(
        ((n: tynode, deps_given, former_given)) =>
          switch (n.kind) {
          /* a type from outside: what it is made of is not this picture's */
          | Ghost => (
              {
                ...n,
                parts: [],
                former: None,
              },
              deps_given,
              true,
            )
          /* a record's fields are in its body; `ctrs` would read as a sum */
          | Alias when n.former == Some("{}") => (
              {
                ...n,
                ctrs: [],
              },
              deps_given,
              former_given,
            )
          | _ => (n, deps_given, former_given)
          },
        raw_nodes,
      );
    let nodes0 = List.map(((n, _, _)) => n, raw_nodes);
    let by_key: Hashtbl.t(string, tynode) = Hashtbl.create(64);
    List.iter(
      (n: tynode) =>
        if (!Hashtbl.mem(by_key, n.key)) {
          Hashtbl.replace(by_key, n.key, n);
        },
      nodes0,
    );
    let find0 = (k: string): option(tynode) => Hashtbl.find_opt(by_key, k);
    /* who uses a node: the functions it is an end of (their module
       paths), the nodes it is a part of, the values of its type */
    let fn_ends: Hashtbl.t(string, list(string)) = Hashtbl.create(64)
    and hosts: Hashtbl.t(string, tynode) = Hashtbl.create(64)
    and holds: Hashtbl.t(string, list(value)) = Hashtbl.create(16);
    let first = (t, k, x) =>
      if (!Hashtbl.mem(t, k)) {
        Hashtbl.replace(t, k, x);
      };
    List.iter((e: edge) => first(fn_ends, e.e_src, e.m_path), edges);
    List.iter((e: edge) => first(fn_ends, e.dst, e.m_path), edges);
    List.iter(
      (n: tynode) => List.iter(p => first(hosts, p, n), n.parts),
      nodes0,
    );
    List.iter(
      (v: value) =>
        Hashtbl.replace(
          holds,
          v.v_key,
          Option.value(Hashtbl.find_opt(holds, v.v_key), ~default=[]) @ [v],
        ),
      values,
    );
    let glyph_of = (n: tynode): option(string) =>
      switch (n.kind, n.former) {
      | (Derived, Some(("()" | "[]" | "?") as glyph)) when n.parts != [] =>
        Some(glyph)
      | _ => None
      };
    /* An unnamed tuple, list or option is a former dot where something
       is made of it or a function uses it: a function's end, or a part
       of a declared type, of an argument tuple or of another dot. One
       that only holds values, or is a part of a type shown as text,
       keeps its text. */
    let dots: Hashtbl.t(string, string) = Hashtbl.create(64);
    let rec dot = (k: string) =>
      switch (find0(k)) {
      | Some(n) when !Hashtbl.mem(dots, k) =>
        switch (glyph_of(n)) {
        | Some(glyph) =>
          Hashtbl.replace(dots, k, glyph);
          List.iter(dot, n.parts);
        | None => ()
        }
      | _ => ()
      };
    List.iter(
      (e: edge) => {
        dot(e.e_src);
        dot(e.dst);
      },
      edges,
    );
    List.iter(
      (n: tynode) =>
        if (n.kind == Alias || n.kind == Product) {
          List.iter(dot, n.parts);
        },
      nodes0,
    );
    /* the module a node with no declaration belongs to: that of the
       first function it is an end of, else of what it is a part of */
    let rec home = (seen: list(string), k: string): list(string) =>
      switch (Hashtbl.find_opt(fn_ends, k), Hashtbl.find_opt(hosts, k)) {
      | (Some(m_path), _) => m_path
      | (None, Some(host)) when host.kind == Alias => host.m_path
      | (None, Some(host)) when !List.mem(host.key, seen) =>
        home([k, ...seen], host.key)
      | _ =>
        switch (Hashtbl.find_opt(holds, k)) {
        | Some([v, ..._]) => v.m_path
        | _ => []
        }
      };
    let is_variable = (n: tynode): bool =>
      n.kind == Derived
      && String.length(n.label) >= 2
      && n.label.[0] == '\''
      && !String.contains(n.label, ' ');
    let producer_sats = List.exists((n: tynode) => n.sat != None, nodes0);
    /* split into one terminal per use-site: a builtin, a type variable */
    let plain_builtin = (k: string): bool =>
      !producer_sats
      && (
        switch (find0(k)) {
        | Some(n) => (n.kind == Builtin || is_variable(n)) && n.sat == None
        | None => false
        }
      );
    let label_of = (k: string): string =>
      switch (find0(k)) {
      | Some(n) => n.label
      | None => display_label(k)
      };
    let sats: ref(list(tynode)) = ref([]);
    let sat_keys = Hashtbl.create(64);
    let ensure_sat =
        (~m_path=[], ~anchor_key: string, ~output: bool, ~dup: string, k)
        : string => {
      let label = label_of(k);
      let key = label ++ "@" ++ dup;
      if (!Hashtbl.mem(sat_keys, key)) {
        Hashtbl.replace(sat_keys, key, ());
        sats :=
          [
            mk_node(
              ~kind=Builtin,
              ~m_path,
              ~sat=Some((anchor_key, output)),
              ~label,
              key,
            ),
            ...sats^,
          ];
      };
      key;
    };
    let value_names = (vs: list(value)): string =>
      String.concat(", ", List.map((v: value) => v.v_label, vs));
    let shown_whole = (n: tynode, label: string): option(string) =>
      n.n_doc == None && label != n.label ? Some(n.label) : n.n_doc;
    let nodes1 =
      List.map(
        ((n: tynode, deps_given, former_given)) => {
          /* how this node is drawn */
          let n =
            switch (n.kind, Hashtbl.find_opt(dots, n.key)) {
            | (Derived, Some(glyph)) => {
                ...n,
                kind: Product,
                label: glyph,
                former: None,
                m_path: home([], n.key),
                n_doc: shown_whole(n, glyph),
              }
            | (Derived, None) =>
              let label =
                switch (
                  Hashtbl.mem(fn_ends, n.key) || Hashtbl.mem(hosts, n.key),
                  Hashtbl.find_opt(holds, n.key),
                ) {
                /* there for its values alone: say which */
                | (false, Some(vs)) =>
                  Abbreviate.ty(value_names(vs) ++ ": " ++ n.label)
                | _ => Abbreviate.ty(n.label)
                };
              {
                /* its text names its components: none are drawn */

                ...n,
                label,
                parts: [],
                n_doc: shown_whole(n, label),
              };
            | (Product, _) when !is_former_key(n.key) => {
                ...n,
                m_path: home([], n.key),
              }
            | (Ghost, _) =>
              let label = Abbreviate.ty(n.label);
              {
                ...n,
                label,
                n_doc: shown_whole(n, label),
              };
            | _ => n
            };
          /* components: a builtin part becomes a terminal of its product
             or of the named type it is a component of */
          let parts =
            List.mapi(
              (i, p) =>
                plain_builtin(p)
                  ? ensure_sat(
                      ~m_path=n.m_path,
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
              ? switch (n.former) {
                | Some("->") => Some({js|→|js})
                | f => f
                }
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
                        m.key != n.key
                        && (
                          m.kind == Alias
                          && (
                            m.key == id
                            || m.key == qname(n.m_path, id)
                            || m.label == id
                            && m.m_path == n.m_path
                          )
                          || m.kind == Ghost
                          && m.key == id
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
                        | Some(m) => !plain_builtin(p) && m.key != n.key
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
    /* a builtin or an outside type that only holds values: say which */
    let nodes1 =
      List.map(
        (n: tynode) =>
          switch (n.kind, Hashtbl.find_opt(holds, n.key)) {
          | (Builtin | Ghost, Some(vs))
              when
                !
                  List.exists(
                    (e: edge) => e.e_src == n.key || e.dst == n.key,
                    edges,
                  )
                && !
                     List.exists(
                       (m: tynode) => List.mem(n.key, m.parts),
                       nodes1,
                     ) => {
              ...n,
              label: Abbreviate.ty(value_names(vs) ++ ": " ++ n.label),
            }
          | _ => n
          },
        nodes1,
      );
    /* what nothing uses is left out: a shared builtin every use of
       which became a terminal, the insides of a type from outside */
    let all = nodes1 @ List.rev(sats^);
    let kept = Hashtbl.create(64);
    let rec keep = (k: string) =>
      if (!Hashtbl.mem(kept, k)) {
        Hashtbl.replace(kept, k, ());
        switch (List.find_opt((n: tynode) => n.key == k, all)) {
        | Some(n) =>
          List.iter(keep, n.parts);
          List.iter(keep, n.deps);
        | None => ()
        };
      };
    List.iter(
      (n: tynode) =>
        if (n.kind == Alias || is_former_key(n.key)) {
          keep(n.key);
        },
      all,
    );
    List.iter(
      (e: edge) => {
        keep(e.e_src);
        keep(e.dst);
      },
      edges,
    );
    List.iter((v: value) => keep(v.v_key), values);
    let nodes2 = List.filter((n: tynode) => Hashtbl.mem(kept, n.key), all);
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
