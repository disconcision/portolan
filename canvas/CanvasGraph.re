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
   - there is no extraction here: a graph arrives as Portolan's graph
     record (graph/graph.re) and `of_graph` makes the picture of it. */

module Id = Haz3lcore.Id;
module Graph = Portolan_graph.Graph;

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

/* PORTOLAN: where a definition lives */
type handle =
  Graph.handle = {
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

let display_label = (key: Graph.key): string =>
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

let is_former_key = (key: Graph.key): bool =>
  String.length(key) >= 3 && String.sub(key, 0, 3) == "{}@";
let former_key = (path: list(string)): Graph.key =>
  "{}@" ++ String.concat(".", path);

/* ---------- PORTOLAN: from the graph record ---------- */

/* PORTOLAN: short labels for long printed types (Hazel cuts a label at 20
   characters; OCaml's inferred types need more care than that). */
module Abbreviate: {
  /* the most characters a label has */
  let limit: int;
  /* A printed type, short enough for a label. */
  let ty: string => string;
} = {
  let limit = 32;
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

  /* characters, not bytes: "…" is three bytes */
  let length = (s: string): int => {
    let n = ref(0);
    String.iter(c => Char.code(c) land 0xC0 != 0x80 ? incr(n) : (), s);
    n^;
  };

  /* "CanvasGraph.tynode" -> "tynode"; "Effect.t" stays: [t] alone
     names nothing */
  let bare = (id: string): string =>
    switch (List.rev(String.split_on_char('.', id))) {
    | [last, _, ..._] when String.length(last) > 1 => last
    | _ => id
    };

  /* what is inside the brackets nested deeper than [keep] becomes "…",
     unless it is short anyway */
  let collapse = (~keep: int, s: string): string => {
    let b = Buffer.create(String.length(s));
    let rec go = (i, stop, depth) =>
      if (i < stop) {
        switch (opens(s.[i]) ? closing(s, i) : None) {
        | Some(close) when close < stop =>
          Buffer.add_char(b, s.[i]);
          if (depth >= keep
              && length(String.sub(s, i + 1, close - i - 1)) > 6) {
            Buffer.add_string(b, dots);
          } else {
            go(i + 1, close, depth + 1);
          };
          Buffer.add_char(b, s.[close]);
          go(close + 1, stop, depth);
        | _ =>
          Buffer.add_char(b, s.[i]);
          go(i + 1, stop, depth);
        };
      };
    go(0, String.length(s), 0);
    Buffer.contents(b);
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
     reduced to their tags; with module paths cut to their last module,
     then dropped; with the inner brackets closed up, then the outer
     ones; cut off. */
  let ty = (s: string): string => {
    let fits = s => length(s) <= limit;
    let steps = [
      variants,
      map_idents(short_path),
      map_idents(bare),
      collapse(~keep=1),
      collapse(~keep=0),
      cut,
    ];
    List.fold_left((s, step) => fits(s) ? s : step(s), s, steps);
  };
};

/* The picture of a graph record.

   The canvas wants a few things the record does not say. They are
   derived here, the way Hazel's extraction does it:

   - satellites: a builtin node (or a type variable) that is a function's
     only argument, its result, or a component is split into one small
     terminal per use-site, docked to the node at the other end;
   - `deps` of a named type: the other named types its body mentions;
   - `former`, where the record gives none: "()" for a type with
     component parts, "+" for a sum;
   - a module's former node ("{}@M"): the host its members cluster on.

   And a few things are drawn differently from how the record says them:

   - an unnamed tuple, list or option (a Derived node with that former)
     that functions use or types are made of is a former dot, as Hazel
     draws a tuple written inline: its glyph, fed by its components;
   - any other unnamed type keeps its text, abbreviated (see Abbreviate);
     the whole of it is in the node's tooltip;
   - a type from outside (Ghost) shows no components of its own;
   - a node that nothing in the picture uses is left out. */
module Read: {
  let graph: Graph.t => t;
} = {
  /* A node of the picture is found by its key: one of the record's, or
     one made here for a terminal ("int@Geo.scale") or a module's former
     ("{}@Geo"). */
  type key = Graph.key;

  let kind = (k: Graph.node_kind): node_kind =>
    switch (k) {
    | Graph.Alias => Alias
    | Graph.Builtin => Builtin
    | Graph.Derived => Derived
    | Graph.Ghost => Ghost
    | Graph.Product => Product
    };

  let status = (s: Graph.test_status): option(TestStatus.t) =>
    switch (s) {
    | Graph.Pass => Some(TestStatus.Pass)
    | Graph.Fail => Some(TestStatus.Fail)
    | Graph.Unknown => None
    };

  /* ---- the record's items, in the canvas's shape ---- */

  /* [deps], [hidden_deps] and [sat] are not in the record: [graph]
     fills them in. */
  let node = (n: Graph.node): tynode => {
    let kind = kind(n.kind);
    {
      key: n.key,
      /* a tuple-formation dot is drawn with its glyph; what it is a tuple
         OF is told by its formation lines (and its key) */
      label: kind == Product && !is_former_key(n.key) ? "()" : n.label,
      m_path: n.m_path,
      n_id: Option.map(_ => n.key, n.at),
      kind,
      /* a record's fields are in its body; `ctrs` would read as a sum */
      ctrs: kind == Alias && n.former == Some("{}") ? [] : n.ctors,
      n_ty: n.n_ty,
      n_doc: n.n_doc,
      n_err: n.n_err,
      deps: [],
      hidden_deps: [],
      /* a type from outside: what it is made of is not this picture's */
      parts: kind == Ghost ? [] : n.parts,
      sat: None,
      former: kind == Ghost ? None : n.former,
    };
  };

  let test_id = (t: Graph.test): Id.t => "test:" ++ t.t_name;

  let test = (t: Graph.test): test_info => {
    t_id: test_id(t),
    status: status(t.t_status),
    t_body: t.t_name,
    t_refs: t.t_refs,
  };

  let edge = (e: Graph.edge): edge => {
    e_name: e.e_name,
    e_label: e.e_label,
    m_path: e.e_m_path,
    e_id: e.e_name,
    e_ty: e.e_ty,
    e_src: e.src,
    dst: e.dst,
    e_doc: e.e_doc,
    e_err: e.e_err,
    e_hole: e.e_hole,
    main: e.main,
    tests: List.map(test, e.tests),
    e_arg_ids: [],
    e_whole_ids: [],
    e_out_id: None,
    e_deps: e.e_deps,
    e_exported: e.e_exported,
  };

  let value = (v: Graph.value): value => {
    v_name: v.v_name,
    v_label: v.v_label,
    m_path: v.v_m_path,
    v_id: v.v_name,
    v_key: v.v_key,
    v_ty: v.v_ty,
    v_err: v.v_err,
    v_def: "",
    v_doc: v.v_doc,
    v_deps: v.v_deps,
    v_exported: v.v_exported,
  };

  /* where each id is declared: nodes, then each function and its tests,
     then values, then loose tests */
  let handles = (g: Graph.t): list((Id.t, handle)) => {
    let of_test = (t: Graph.test) => (test_id(t), t.t_at);
    List.concat([
      List.filter_map(
        (n: Graph.node) => Option.map(h => (n.key, h), n.at),
        g.nodes,
      ),
      List.concat_map(
        (e: Graph.edge) =>
          [(e.e_name, e.e_at), ...List.map(of_test, e.tests)],
        g.edges,
      ),
      List.map((v: Graph.value) => (v.v_name, v.v_at), g.values),
      List.map(of_test, g.loose_tests),
    ]);
  };

  /* ---- what the picture adds ---- */

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

  /* the glyph an unnamed tuple, list or option is drawn as */
  let glyph_of = (n: tynode): option(string) =>
    switch (n.kind, n.former) {
    | (Derived, Some(("()" | "[]" | "?") as glyph)) when n.parts != [] =>
      Some(glyph)
    | _ => None
    };

  let is_variable = (n: tynode): bool =>
    n.kind == Derived
    && String.length(n.label) >= 2
    && n.label.[0] == '\''
    && !String.contains(n.label, ' ');

  let value_names = (vs: list(value)): string =>
    String.concat(", ", List.map((v: value) => v.v_label, vs));

  /* the tooltip of a node drawn as [label]: its whole text, if the label
     is not that */
  let shown_whole = (n: tynode, label: string): option(string) =>
    n.n_doc == None && label != n.label ? Some(n.label) : n.n_doc;

  let graph = (g: Graph.t): t => {
    let nodes0 = List.map(node, g.nodes);
    let edges = List.map(edge, g.edges);
    let values = List.map(value, g.values);
    let loose_tests = List.map(test, g.loose_tests);
    let all_tests =
      List.concat_map((e: edge) => e.tests, edges) @ loose_tests;

    let by_key: Hashtbl.t(key, tynode) = Hashtbl.create(64);
    List.iter(
      (n: tynode) =>
        if (!Hashtbl.mem(by_key, n.key)) {
          Hashtbl.replace(by_key, n.key, n);
        },
      nodes0,
    );
    let find0 = (k: key): option(tynode) => Hashtbl.find_opt(by_key, k);
    /* who uses a node: the functions it is an end of (their module
       paths), the nodes it is a part of, the values of its type */
    let fn_ends: Hashtbl.t(key, list(string)) = Hashtbl.create(64)
    and hosts: Hashtbl.t(key, tynode) = Hashtbl.create(64)
    and holds: Hashtbl.t(key, list(value)) = Hashtbl.create(16);
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
    /* An unnamed tuple, list or option is a former dot where something
       is made of it or a function uses it: a function's end, or a part
       of a declared type, of an argument tuple or of another dot. One
       that only holds values, or is a part of a type shown as text,
       keeps its text. */
    let dots: Hashtbl.t(key, string) = Hashtbl.create(64);
    let rec dot = (k: key) =>
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
    /* A tuple or a dot is in the module of the first function it is an
       end of. One that is only a part of types stays outside: a module's
       former has room for few members around it. */
    let home = (k: key): list(string) =>
      Option.value(Hashtbl.find_opt(fn_ends, k), ~default=[]);
    /* split into one terminal per use-site: a builtin, a type variable */
    let plain_builtin = (k: key): bool =>
      switch (find0(k)) {
      | Some(n) => n.kind == Builtin || is_variable(n)
      | None => false
      };
    let label_of = (k: key): string =>
      switch (find0(k)) {
      | Some(n) => n.label
      | None => display_label(k)
      };
    let sats: ref(list(tynode)) = ref([]);
    let sat_keys = Hashtbl.create(64);
    /* the terminal of [k] at one use-site, which [dup] tells apart */
    let ensure_sat =
        (~m_path=[], ~anchor_key: key, ~output: bool, ~dup: string, k: key)
        : key => {
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
    let nodes1 =
      List.map(
        (n: tynode) => {
          /* how this node is drawn */
          let n =
            switch (n.kind, Hashtbl.find_opt(dots, n.key)) {
            | (Derived, Some(glyph)) => {
                ...n,
                kind: Product,
                label: glyph,
                former: None,
                m_path: home(n.key),
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
                m_path: home(n.key),
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
            )
            /* one line per part: a record with five [string list] fields
               would otherwise count as five lines for the layout to
               keep other nodes off */
            |> List.fold_left(
                 (acc, p) => List.mem(p, acc) ? acc : acc @ [p],
                 [],
               );
          let former =
            switch (n.former) {
            | Some("->") => Some({js|→|js})
            | Some(_) as given => given
            | None =>
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
            };
          let deps: list(key) =
            switch (n.kind) {
            | Product =>
              List.filter(p => !plain_builtin(p), n.parts)
              |> List.sort_uniq(compare)
            | Alias =>
              /* the named types the body mentions */
              let named = (id: string): option(key) =>
                List.find_map(
                  (m: tynode) =>
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
                  nodes0,
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
            };
          {
            ...n,
            parts,
            former,
            deps,
            /* shown through the formation lines of its parts instead */
            hidden_deps: n.kind == Alias && parts != [] ? deps : [],
          };
        },
        nodes0,
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
    let rec keep = (k: key) =>
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
      all_tests,
      last_def: None,
      livelit_defs: [],
      handles: handles(g),
    };
  };
};

/* The picture of a graph record: see Read. */
let of_graph: Graph.t => t = Read.graph;

/* [of_graph] of a record's JSON; [Error] says what is not a graph record
   about it. */
let of_json = (j: Yojson.Safe.t): result(t, string) =>
  Result.map(of_graph, Graph.of_json(j));
let of_string = (s: string): result(t, string) =>
  Result.map(of_graph, Graph.of_string(s));

let handle_of = (g: t, id: Id.t): option(handle) =>
  List.assoc_opt(id, g.handles);
