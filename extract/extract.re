/* Reads a dune project's .cmt files and builds the Portolan graph.

   Two passes over the typed trees:
   1. [collect]: flatten every module (and nested module) into a list of
      items, and record which names the project declares, so that a
      reference from one file to another can be resolved.
   2. [emit]: turn each item into a node, an edge, a value or a test.

   Nothing here needs the compiler's environment or load path: paths are
   resolved by hand against the names collected in pass 1.

   A module that cannot be read this time (it does not type-check, or its
   annotations are older than its source) can be carried over from an
   earlier graph: see "carrying over" below. */

module G = Portolan_graph.Graph;
open Typedtree;

/* ---------- strings and files ---------- */

let (/\/) = Filename.concat;

/* "Vector__Scene" -> ["Vector"; "Scene"] */
let split_double_underscore = (s: string): list(string) => {
  let n = String.length(s);
  let rec go = (start, i, acc) =>
    if (i + 1 >= n) {
      List.rev([String.sub(s, start, n - start), ...acc]);
    } else if (s.[i] == '_' && s.[i + 1] == '_' && i > start) {
      go(i + 2, i + 2, [String.sub(s, start, i - start), ...acc]);
    } else {
      go(start, i + 1, acc);
    };

  List.filter(part => part != "", go(0, 0, []));
};

/* Collapse every run of whitespace into one space. */
let one_line = (s: string): string =>
  String.split_on_char(
    ' ',
    String.map(
      fun
      | '\n'
      | '\t'
      | '\r' => ' '
      | c => c,
      s,
    ),
  )
  |> List.filter(w => w != "")
  |> String.concat(" ");

let contains = (~sub: string, s: string): bool => {
  let n = String.length(sub)
  and m = String.length(s);
  let rec at = i => i + n <= m && (String.sub(s, i, n) == sub || at(i + 1));
  at(0);
};

let dedup = (l: list(string)): list(string) =>
  List.fold_left(
    (acc, x) =>
      if (List.mem(x, acc)) {
        acc;
      } else {
        [x, ...acc];
      },
    [],
    l,
  )
  |> List.rev;

let qualify = (mpath: list(string), name: string): string =>
  String.concat(".", mpath @ [name]);

/* Every .cmt / .cmti under [dir], sorted. */
let rec find_cmts = (dir: string): list(string) =>
  switch (Sys.readdir(dir)) {
  | exception (Sys_error(_)) => []
  | entries =>
    Array.sort(compare, entries);
    Array.to_list(entries)
    |> List.concat_map(entry => {
         let path = dir /\/ entry;
         if (Sys.is_directory(path)) {
           find_cmts(path);
         } else if (Filename.check_suffix(entry, ".cmt")
                    || Filename.check_suffix(entry, ".cmti")) {
           [path];
         } else {
           [];
         };
       });
  };

/* ---------- what pass 1 collects ---------- */

/* What an interface lets through. [Only sg]: just what [sg] lists. */
type vis =
  | All
  | Only(Types.signature)
  | Nothing;

/* Where an item sits: its enclosing modules, and what they export. */
type scope = {
  mpath: list(string),
  vis,
};

type item =
  | Type_item(scope, type_declaration)
  | Value_item(scope, value_binding, Location.t) /* loc of the whole item */
  | Prim_item(scope, value_description); /* external f : ... */

/* One compilation unit (one .ml file). The three tables map the idents
   the unit declares to their qualified names; idents are only unique
   within a unit, so each unit has its own. */
type unit_info = {
  u_modname: string, /* as compiled: "Vector__Scene" */
  u_mpath: list(string), /* as shown: ["Scene"] */
  u_file: string, /* relative to the root: "lib/scene.ml" */
  u_str: structure,
  u_vis: vis,
  u_types: Ident.Tbl.t(string),
  u_mods: Ident.Tbl.t(string),
  u_vals: Ident.Tbl.t(string),
  u_mtys: Ident.Tbl.t(Types.signature), /* [module type S = sig ... end] */
  mutable u_items: list(item) /* reversed */
};

/* A test before the graph is assembled. */
type found_test = {
  ft_test: G.test,
  ft_uses: list(string), /* the project names its body mentions */
  /* For an Alcotest case, which is written apart from what it tests: */
  ft_alcotest: option(alcotest),
}
and alcotest = {
  /* the qualified name of its file's module and a ".": the definitions
     under it are helpers, looked through to what they use */
  a_helpers: string,
  /* its suite and its place in it, when the suite is written out */
  a_case: option((string, int)),
  /* the top-level function that is its body, when it is one */
  a_fn: option(string),
};

/* A top-level definition before the graph is assembled: [mentions] are
   the project names its body refers to, not yet filtered. */
type def =
  | Edge(G.edge, list(string))
  | Value(G.value, list(string))
  /* from an earlier graph: what they mention is already in the record */
  | Carried_edge(G.edge)
  | Carried_value(G.value);

type ctx = {
  failing: option(list((string, int))),
  units: Hashtbl.t(string, unit_info), /* by compiled name */
  /* dune's alias modules: ("Vector", "Scene") -> "Vector__Scene" */
  wrappers: Hashtbl.t((string, string), string),
  /* [module S = Scene] inside the project: "Other.S" -> "Scene" */
  mod_aliases: Hashtbl.t(string, string),
  /* every type the project declares, by node key */
  type_decls: Hashtbl.t(string, (unit_info, scope, type_declaration)),
  /* every top-level value the project binds, by qualified name */
  def_names: Hashtbl.t(string, unit),
  /* modules carried over from an earlier graph, by compiled name: the
     name the graph shows them under ("Geo") */
  carried_units: Hashtbl.t(string, string),
  /* keys of the types those modules declare */
  carried_types: Hashtbl.t(string, unit),
  /* the earlier graph's nodes, by key (the carried ones as they are now) */
  previous_nodes: Hashtbl.t(string, G.node),
  seen: Hashtbl.t(string, unit), /* node keys already emitted */
  mutable nodes: list(G.node), /* reversed */
  mutable defs: list(def), /* reversed */
  mutable tests: list(found_test) /* reversed */
};

/* ---------- resolving paths to project names ---------- */

let unit_name = (ctx: ctx, modname: string): option(string) =>
  switch (Hashtbl.find_opt(ctx.units, modname)) {
  | Some(u) => Some(String.concat(".", u.u_mpath))
  | None => Hashtbl.find_opt(ctx.carried_units, modname)
  };

/* The qualified name of the project module a path denotes, if any. */
let rec module_prefix = (ctx: ctx, u: unit_info, p: Path.t): option(string) =>
  switch (p) {
  | Pident(id) =>
    switch (Ident.Tbl.find_opt(u.u_mods, id)) {
    | Some(prefix) => Some(prefix)
    | None =>
      if (Ident.persistent(id)) {
        unit_name(ctx, Ident.name(id));
      } else {
        None;
      }
    }
  | [@implicit_arity] Pdot(Pident(w), sub)
      when Hashtbl.mem(ctx.wrappers, (Ident.name(w), sub)) =>
    /* Vector.Scene, through dune's alias module */
    unit_name(ctx, Hashtbl.find(ctx.wrappers, (Ident.name(w), sub)))
  | [@implicit_arity] Pdot(m, sub) =>
    module_prefix(ctx, u, m)
    |> Option.map(prefix => {
         let q = prefix ++ "." ++ sub;
         Option.value(Hashtbl.find_opt(ctx.mod_aliases, q), ~default=q);
       })
  | Papply(_)
  | Pextra_ty(_) => None
  };

/* The node key of a project type, if the path names one. [from] is the
   module the reference is written in. */
let project_type =
    (ctx: ctx, u: unit_info, ~from: list(string), p: Path.t): option(string) => {
  let declared = key =>
    if (Hashtbl.mem(ctx.type_decls, key)
        || Hashtbl.mem(ctx.carried_types, key)) {
      Some(key);
    } else {
      None;
    };
  switch (p) {
  | Pident(id) =>
    switch (Ident.Tbl.find_opt(u.u_types, id)) {
    | Some(key) => Some(key)
    | None =>
      /* An ident pass 1 did not see (it came through an include, say):
         look the name up in the enclosing modules, innermost first. */
      let rec outward = (
        fun
        | [] => None
        | [_, ...rest] as rev_mpath =>
          switch (declared(qualify(List.rev(rev_mpath), Ident.name(id)))) {
          | Some(key) => Some(key)
          | None => outward(rest)
          }
      );

      outward(List.rev(from));
    }
  | [@implicit_arity] Pdot(m, name) =>
    Option.bind(module_prefix(ctx, u, m), prefix =>
      declared(prefix ++ "." ++ name)
    )
  | Papply(_)
  | Pextra_ty(_) => None
  };
};

/* The qualified name of a project top-level value, if the path names one.
   Local variables are idents pass 1 never registered, so they fall out. */
let project_value = (ctx: ctx, u: unit_info, p: Path.t): option(string) =>
  switch (p) {
  | Pident(id) => Ident.Tbl.find_opt(u.u_vals, id)
  | [@implicit_arity] Pdot(m, name) =>
    Option.bind(
      module_prefix(ctx, u, m),
      prefix => {
        let q = prefix ++ "." ++ name;
        if (Hashtbl.mem(ctx.def_names, q)) {
          Some(q);
        } else {
          None;
        };
      },
    )
  | Papply(_)
  | Pextra_ty(_) => None
  };

let is_predef = (p: Path.t): bool =>
  switch (p) {
  | Pident(id) => Ident.is_predef(id)
  | _ => false
  };

/* A type from outside the project, named the way one would write it:
   "Stdlib.Format.formatter" -> "Format.formatter", "Lib__Mod.t" -> "Lib.Mod.t". */
let outside_name = (p: Path.t): string => {
  let name = String.concat(".", split_double_underscore(Path.name(p)));
  let stdlib = "Stdlib.";
  if (String.starts_with(~prefix=stdlib, name)) {
    String.sub(
      name,
      String.length(stdlib),
      String.length(name) - String.length(stdlib),
    );
  } else {
    name;
  };
};

/* "Scene.Sub.t" as written from inside [from]: the enclosing modules the
   two have in common are dropped. */
let relative = (~from: list(string), key: string): string => {
  let rec drop = (parts, from) =>
    switch (parts, from) {
    | ([p, ...[_, ..._] as parts'], [f, ...from']) when p == f =>
      drop(parts', from')
    | _ => parts
    };

  String.concat(".", drop(String.split_on_char('.', key), from));
};

/* ---------- printing types ---------- */

/* Printtyp prints paths as the type checker stored them ([Vector__Scene.shape],
   or a bare [shape] whose meaning depends on the file). To choose the names
   ourselves we print a copy of the type in which every named type is
   replaced by an ident carrying the name we want. Type variables are shared
   with the original, so they keep their names; object and polymorphic
   variant types are left as they are. */
let rec rename =
        (name_of: Path.t => option(string), ty: Types.type_expr)
        : Types.type_expr => {
  let rename = rename(name_of);
  switch (Types.get_desc(ty)) {
  | [@implicit_arity] Tconstr(p, args, _) =>
    let p =
      switch (name_of(p)) {
      | Some(name) => Path.Pident(Ident.create_persistent(name))
      | None => p
      };

    Btype.newgenty(
      [@implicit_arity] Tconstr(p, List.map(rename, args), ref(Types.Mnil)),
    );
  | [@implicit_arity] Tarrow(label, a, b, c) =>
    Btype.newgenty(
      [@implicit_arity] Tarrow(label, rename(a), rename(b), c),
    )
  | Ttuple(l) => Btype.newgenty(Ttuple(List.map(rename, l)))
  | [@implicit_arity] Tpoly(t, vars) =>
    Btype.newgenty([@implicit_arity] Tpoly(rename(t), vars))
  | _ => ty
  };
};

/* The name a path is printed with: qualified ("Scene.shape", for keys) or
   as written from [from] ("shape", for labels). Built-in types keep their
   own printing. */
let name_of =
    (
      ctx: ctx,
      u: unit_info,
      ~qualified: bool,
      ~from: list(string),
      p: Path.t,
    )
    : option(string) =>
  if (is_predef(p)) {
    None;
  } else {
    switch (project_type(ctx, u, ~from, p)) {
    | Some(key) =>
      Some(
        if (qualified) {
          key;
        } else {
          relative(~from, key);
        },
      )
    | None => Some(outside_name(p))
    };
  };

/* Run a Format printer onto one line. */
let to_line = (pp: (Format.formatter, 'a) => unit, x: 'a): string => {
  let buffer = Buffer.create(80);
  let ppf = Format.formatter_of_buffer(buffer);
  Format.pp_set_margin(ppf, 1_000_000);
  pp(ppf, x);
  Format.pp_print_flush(ppf, ());
  one_line(Buffer.contents(buffer));
};

let print_type =
    (
      ctx: ctx,
      u: unit_info,
      ~qualified: bool,
      ~from: list(string),
      ty: Types.type_expr,
    )
    : string => {
  /* [reset] forgets earlier idents; [type_scheme] restarts variables at 'a. */
  Printtyp.reset();
  to_line(
    Printtyp.type_scheme,
    rename(name_of(ctx, u, ~qualified, ~from), ty),
  );
};

/* The right-hand side of a type declaration, [None] if it has none. */
let decl_rhs =
    (ctx: ctx, u: unit_info, ~from: list(string), d: type_declaration)
    : option(string) => {
  let ty = rename(name_of(ctx, u, ~qualified=false, ~from));
  let label = (l: Types.label_declaration) => {
    ...l,
    ld_type: ty(l.ld_type),
  };
  let ctor = (c: Types.constructor_declaration) => {
    ...c,
    cd_args:
      switch (c.cd_args) {
      | Cstr_tuple(l) => Cstr_tuple(List.map(ty, l))
      | Cstr_record(l) => Cstr_record(List.map(label, l))
      },
    cd_res: Option.map(ty, c.cd_res),
  };

  let decl = d.typ_type;
  let decl = {
    ...decl,
    type_manifest: Option.map(ty, decl.type_manifest),
    type_kind:
      switch (decl.type_kind) {
      | [@implicit_arity] Type_record(l, r) =>
        [@implicit_arity] Type_record(List.map(label, l), r)
      | [@implicit_arity] Type_variant(l, r) =>
        [@implicit_arity] Type_variant(List.map(ctor, l), r)
      | (Type_abstract(_) | Type_open) as k => k
      },
  };

  Printtyp.reset();
  /* Prints "type shape = Circle of ... | ...": keep what follows the "=". */
  let printed = to_line(Printtyp.type_declaration(d.typ_id), decl);
  let sep = " = ";
  let rec find = i =>
    if (i + String.length(sep) > String.length(printed)) {
      None;
    } else if (String.sub(printed, i, String.length(sep)) == sep) {
      Some(i + String.length(sep));
    } else {
      find(i + 1);
    };

  find(0)
  |> Option.map(start => {
       let rhs = String.sub(printed, start, String.length(printed) - start);
       if (String.length(rhs) > 200) {
         String.sub(rhs, 0, 200) ++ " ...";
       } else {
         rhs;
       };
     });
};

/* ---------- handles, docs, visibility ---------- */

let handle =
    (u: unit_info, mpath: list(string), name: string, loc: Location.t)
    : G.handle => {
  file: u.u_file,
  path: String.concat("/", mpath @ [name]),
  line: loc.loc_start.pos_lnum,
  end_line: loc.loc_end.pos_lnum,
};

/* A doc comment reaches the typed tree as [@ocaml.doc "..."]. */
let doc_of = (attrs: attributes): option(string) =>
  List.find_map(
    (a: Parsetree.attribute) =>
      switch (a.attr_name.txt, a.attr_payload) {
      | (
          "ocaml.doc" | "doc",
          PStr([
            {
              pstr_desc:
                [@implicit_arity]
                Pstr_eval(
                  {
                    pexp_desc:
                      Pexp_constant(
                        [@implicit_arity] Pconst_string(s, _, _),
                      ),
                    _,
                  },
                  _,
                ),
              _,
            },
          ]),
        ) =>
        Some(String.trim(s))
      | _ => None
      },
    attrs,
  );

/* Doc comments are often written in the .mli only: look there too. */
let sig_doc = (vis: vis, ~is_type: bool, name: string): option(string) =>
  switch (vis) {
  | All
  | Nothing => None
  | Only(sg) =>
    List.find_map(
      fun
      | [@implicit_arity] Types.Sig_value(id, vd, _)
          when !is_type && Ident.name(id) == name =>
        doc_of(vd.val_attributes)
      | [@implicit_arity] Types.Sig_type(id, decl, _, _)
          when is_type && Ident.name(id) == name =>
        doc_of(decl.type_attributes)
      | _ => None,
      sg,
    )
  };

let first_some = (a: option('a), b: option('a)): option('a) =>
  switch (a) {
  | Some(_) => a
  | None => b
  };

let value_exported = (vis: vis, name: string): bool =>
  switch (vis) {
  | All => true
  | Nothing => false
  | Only(sg) =>
    List.exists(
      fun
      | [@implicit_arity] Types.Sig_value(id, _, _) => Ident.name(id) == name
      | _ => false,
      sg,
    )
  };

/* (exported, abstract) */
let type_visibility = (vis: vis, name: string): (bool, bool) =>
  switch (vis) {
  | All => (true, false)
  | Nothing => (false, false)
  | Only(sg) =>
    let find = (
      fun
      | [@implicit_arity] Types.Sig_type(id, decl, _, _)
          when Ident.name(id) == name =>
        Some(decl)
      | _ => None
    );

    switch (List.find_map(find, sg)) {
    | None => (false, false)
    | Some(decl) =>
      switch (decl.type_kind, decl.type_manifest) {
      | (Type_abstract(_), None) => (true, true)
      | _ => (true, false)
      }
    };
  };

/* The items of a module type, when it is written out or is a module type
   declared earlier in the same file. One from elsewhere cannot be looked
   into without the compiler's environment. */
let signature_of =
    (u: unit_info, mty: Types.module_type): option(Types.signature) =>
  switch (mty) {
  | Mty_signature(sg) => Some(sg)
  | Mty_ident(Pident(id)) => Ident.Tbl.find_opt(u.u_mtys, id)
  | _ => None
  };

/* What is visible of submodule [name]. If its signature cannot be looked
   into, treat it as fully exported. */
let sub_vis = (u: unit_info, vis: vis, name: string): vis =>
  switch (vis) {
  | All => All
  | Nothing => Nothing
  | Only(sg) =>
    let find = (
      fun
      | [@implicit_arity] Types.Sig_module(id, _, md, _, _)
          when Ident.name(id) == name =>
        Some(md.md_type)
      | _ => None
    );

    switch (List.find_map(find, sg)) {
    | None => Nothing
    | Some(mty) =>
      switch (signature_of(u, mty)) {
      | Some(sg) => Only(sg)
      | None => All
      }
    };
  };

/* [(M : S)] and [module M : S = ...]: peel the constraints off, narrowing
   what is visible by each signature. The outer restriction wins. */
let rec strip_constraint =
        (u: unit_info, me: module_expr, vis: vis): (module_expr, vis) =>
  switch (me.mod_desc) {
  | [@implicit_arity] Tmod_constraint(inner, mty, _, _) =>
    let vis =
      switch (vis, signature_of(u, mty)) {
      | (All, Some(sg)) => Only(sg)
      | _ => vis
      };

    strip_constraint(u, inner, vis);
  | _ => (me, vis)
  };

/* ---------- pass 1: collect ---------- */

/* [within] is where the items are when they do not say: a ppx writes
   [include struct ... end] after the type it derives from, and may leave
   what is inside without a location. */
let rec collect =
        (
          ~within: option(Location.t)=?,
          ctx: ctx,
          u: unit_info,
          scope: scope,
          items: list(structure_item),
        )
        : unit => {
  let push = item => u.u_items = [item, ...u.u_items];
  let placed = (loc: Location.t): Location.t =>
    switch (within) {
    | Some(around) when loc.loc_start.pos_lnum == 0 => around
    | _ => loc
    };
  let declare_value = (id, name) => {
    let q = qualify(scope.mpath, name);
    Ident.Tbl.replace(u.u_vals, id, q);
    Hashtbl.replace(ctx.def_names, q, ());
  };

  List.iter(
    (si: structure_item) =>
      switch (si.str_desc) {
      | [@implicit_arity] Tstr_type(_, decls) =>
        List.iter(
          (d: type_declaration) => {
            let key = qualify(scope.mpath, d.typ_name.txt);
            Ident.Tbl.replace(u.u_types, d.typ_id, key);
            Hashtbl.replace(ctx.type_decls, key, (u, scope, d));
            push([@implicit_arity] Type_item(scope, d));
          },
          decls,
        )
      | [@implicit_arity] Tstr_value(_, bindings) =>
        let several = List.length(bindings) > 1;
        List.iter(
          (vb: value_binding) => {
            List.iter(
              ((id, name: Location.loc(string), _, _)) =>
                declare_value(id, name.txt),
              pat_bound_idents_full(vb.vb_pat),
            );
            let loc =
              if (several) {
                vb.vb_loc;
              } else {
                si.str_loc;
              };
            push([@implicit_arity] Value_item(scope, vb, placed(loc)));
          },
          bindings,
        );
      | Tstr_primitive(vd) =>
        declare_value(vd.val_id, vd.val_name.txt);
        push([@implicit_arity] Prim_item(scope, vd));
      | Tstr_modtype({
          mtd_id,
          mtd_type: Some({mty_type: Mty_signature(sg), _}),
          _,
        }) =>
        Ident.Tbl.replace(u.u_mtys, mtd_id, sg)
      | Tstr_module(mb) => collect_module(ctx, u, scope, mb)
      | Tstr_recmodule(mbs) => List.iter(collect_module(ctx, u, scope), mbs)
      | Tstr_include({incl_mod, incl_type, _}) =>
        /* [include struct ... end] is walked in place. The included names
           get fresh idents in the enclosing module ([incl_type]): map
           those to the same qualified names. Other includes are skipped. */
        switch (strip_constraint(u, incl_mod, scope.vis)) {
        | ({mod_desc: Tmod_structure(s), _}, _) =>
          collect(~within=placed(si.str_loc), ctx, u, scope, s.str_items);
          List.iter(
            fun
            | [@implicit_arity] Types.Sig_value(id, _, _) => {
                let q = qualify(scope.mpath, Ident.name(id));
                if (Hashtbl.mem(ctx.def_names, q)) {
                  Ident.Tbl.replace(u.u_vals, id, q);
                };
              }
            | [@implicit_arity] Sig_type(id, _, _, _) => {
                let q = qualify(scope.mpath, Ident.name(id));
                if (Hashtbl.mem(ctx.type_decls, q)) {
                  Ident.Tbl.replace(u.u_types, id, q);
                };
              }
            | [@implicit_arity] Sig_module(id, _, _, _, _) =>
              Ident.Tbl.replace(
                u.u_mods,
                id,
                qualify(scope.mpath, Ident.name(id)),
              )
            | _ => (),
            incl_type,
          );
        | _ => ()
        }
      | _ => ()
      },
    items,
  );
}

and collect_module =
    (ctx: ctx, u: unit_info, scope: scope, mb: module_binding): unit =>
  switch (mb.mb_id, mb.mb_name.txt) {
  | (Some(id), Some(name)) =>
    let prefix = qualify(scope.mpath, name);
    let (me, vis) =
      strip_constraint(u, mb.mb_expr, sub_vis(u, scope.vis, name));
    switch (me.mod_desc) {
    | Tmod_structure(s) =>
      Ident.Tbl.replace(u.u_mods, id, prefix);
      collect(
        ctx,
        u,
        {
          mpath: scope.mpath @ [name],
          vis,
        },
        s.str_items,
      );
    | [@implicit_arity] Tmod_ident(p, _) =>
      /* [module S = Scene]: S is another name for a project module */
      switch (module_prefix(ctx, u, p)) {
      | Some(target) =>
        Ident.Tbl.replace(u.u_mods, id, target);
        Hashtbl.replace(ctx.mod_aliases, prefix, target);
      | None => ()
      }
    | _ => () /* functors, applications, first-class modules: not followed */
    };
  | _ => ()
  }; /* module _ = ... */

/* ---------- nodes ---------- */

let add_node = (ctx: ctx, n: G.node): unit => {
  Hashtbl.replace(ctx.seen, n.key, ());
  ctx.nodes = [n, ...ctx.nodes];
};

/* A node that is not a declaration of the project. */
let plain_node = (~former=None, ~key, ~label, ~kind, ~parts, ()): G.node => {
  key,
  label,
  m_path: [],
  kind,
  ctors: [],
  n_ty: None,
  n_doc: None,
  n_err: false,
  parts,
  former,
  abstract: false,
  exported: true,
  at: None,
};

let ctors_of = (d: type_declaration): list(string) =>
  switch (d.typ_kind, d.typ_manifest) {
  | (Ttype_variant(cds), _) =>
    List.map((c: constructor_declaration) => c.cd_name.txt, cds)
  | (Ttype_record(lds), _) =>
    List.map((l: label_declaration) => l.ld_name.txt, lds)
  | (_, Some({ctyp_desc: [@implicit_arity] Ttyp_variant(fields, _, _), _})) =>
    /* type t = [ `A | `B ] */
    List.filter_map(
      (f: row_field) =>
        switch (f.rf_desc) {
        | [@implicit_arity] Ttag(tag, _, _) => Some(tag.txt)
        | Tinherit(_) => None
        },
      fields,
    )
  | _ => []
  };

let builtin = (ctx: ctx, name: string): string => {
  if (!Hashtbl.mem(ctx.seen, name)) {
    add_node(
      ctx,
      plain_node(~key=name, ~label=name, ~kind=Builtin, ~parts=[], ()),
    );
  };
  name;
};

let ghost = (ctx: ctx, name: string): string => {
  if (!Hashtbl.mem(ctx.seen, name)) {
    add_node(
      ctx,
      plain_node(~key=name, ~label=name, ~kind=Ghost, ~parts=[], ()),
    );
  };
  name;
};

let is_variable = (ty: Types.type_expr): bool =>
  switch (Types.get_desc(ty)) {
  | Tvar(_)
  | Tunivar(_) => true
  | _ => false
  };

/* [a -> ?b:c -> d] as ([a; c], d). An optional argument is stored as
   [c option]: unwrap it. */
let rec flatten_arrow =
        (ty: Types.type_expr): (list(Types.type_expr), Types.type_expr) =>
  switch (Types.get_desc(ty)) {
  | [@implicit_arity] Tpoly(t, _) => flatten_arrow(t)
  | [@implicit_arity] Tarrow(label, arg, rest, _) =>
    let arg =
      switch (label, Types.get_desc(arg)) {
      | (Optional(_), [@implicit_arity] Tconstr(p, [t], _))
          when Path.same(p, Predef.path_option) => t
      | _ => arg
      };

    let (args, result) = flatten_arrow(rest);
    ([arg, ...args], result);
  | _ => ([], ty)
  };

/* Emit the Alias node of a declared type, the first time it is reached:
   at its declaration, or at an earlier reference from another file. Its
   component nodes follow it. */
let rec touch_alias = (ctx: ctx, key: string): unit =>
  if (!Hashtbl.mem(ctx.seen, key)) {
    switch (Hashtbl.find_opt(ctx.type_decls, key)) {
    | None => add_previous(ctx, key) /* declared by a carried module */
    | Some((u, scope, d)) =>
      let name = d.typ_name.txt;
      let (exported, abstract) = type_visibility(scope.vis, name);
      /* Seen from here on, so that a type that is a part of itself
         ([type t = { next : t option }]) stops. The parts' nodes are
         collected aside and put after this one. */
      Hashtbl.replace(ctx.seen, key, ());
      let before = ctx.nodes;
      ctx.nodes = [];
      let (former, parts) = formed_from(ctx, u, ~from=scope.mpath, d);
      let after = ctx.nodes;
      let node: G.node = {
        key,
        label: name,
        m_path: scope.mpath,
        kind: Alias,
        ctors: ctors_of(d),
        n_ty: decl_rhs(ctx, u, ~from=scope.mpath, d),
        n_doc:
          first_some(
            doc_of(d.typ_attributes),
            sig_doc(scope.vis, ~is_type=true, name),
          ),
        n_err: false,
        parts,
        former,
        abstract,
        exported,
        at: Some(handle(u, scope.mpath, name, d.typ_loc)),
      };
      ctx.nodes = after @ [node, ...before];
    };
  }

/* A node of the earlier graph, with the nodes it is made of, in the
   order pass 2 emits them: a declared type before its parts, any other
   node after. A type that is no longer declared anywhere becomes a
   Ghost, so that what refers to it stays attached. */
and add_previous = (ctx: ctx, key: string): unit =>
  if (!Hashtbl.mem(ctx.seen, key)) {
    if (Hashtbl.mem(ctx.type_decls, key)) {
      touch_alias(ctx, key);
    } else {
      switch (Hashtbl.find_opt(ctx.previous_nodes, key)) {
      | Some(n) when n.kind == Alias && Hashtbl.mem(ctx.carried_types, key) =>
        add_node(ctx, n);
        List.iter(add_previous(ctx), n.parts);
      | Some(n) when n.kind != Alias =>
        Hashtbl.replace(ctx.seen, key, ());
        List.iter(add_previous(ctx), n.parts);
        ctx.nodes = [n, ...ctx.nodes];
      | _ =>
        add_node(
          ctx,
          plain_node(~key, ~label=key, ~kind=Ghost, ~parts=[], ()),
        )
      };
    };
  }

/* What a declared type is formed with, and from which nodes. */
and formed_from =
    (ctx: ctx, u: unit_info, ~from: list(string), d: type_declaration)
    : (option(string), list(string)) =>
  switch (d.typ_type.type_kind, d.typ_type.type_manifest) {
  | ([@implicit_arity] Type_record(labels, _), _) => (
      Some("{}"),
      List.map(
        (l: Types.label_declaration) =>
          node_of_type(ctx, u, ~from, l.ld_type),
        labels,
      ),
    )
  | (Type_abstract(_), Some(ty)) =>
    switch (Types.get_desc(ty)) {
    /* [type json = Yojson.Safe.t]: another name for one type */
    | [@implicit_arity] Tconstr(_, [], _) => (
        Some("="),
        [node_of_type(ctx, u, ~from, ty)],
      )
    | _ => formed(ctx, u, ~from, ty)
    }
  | _ => (None, [])
  }

/* What a type expression is formed with, and from which nodes: a tuple
   "()", a list or an array "[]", an option "?", a function "->" (its
   arguments, then its result). Any other type applied to arguments has
   no former: its parts are the type itself, when the project declares
   it, and the arguments. Type variables are not parts. */
and formed =
    (ctx: ctx, u: unit_info, ~from: list(string), ty: Types.type_expr)
    : (option(string), list(string)) => {
  let nodes = l =>
    List.filter(t => !is_variable(t), l)
    |> List.map(node_of_type(ctx, u, ~from));
  let is = (p, predef) => Path.same(p, predef);
  switch (Types.get_desc(ty)) {
  | [@implicit_arity] Tpoly(t, _) => formed(ctx, u, ~from, t)
  | Ttuple(l) => (Some("()"), nodes(l))
  | Tarrow(_) =>
    let (args, result) = flatten_arrow(ty);
    (Some("->"), nodes(args @ [result]));
  | [@implicit_arity] Tconstr(p, [arg], _)
      when is(p, Predef.path_list) || is(p, Predef.path_array) => (
      Some("[]"),
      nodes([arg]),
    )
  | [@implicit_arity] Tconstr(p, [arg], _) when is(p, Predef.path_option) => (
      Some("?"),
      nodes([arg]),
    )
  | [@implicit_arity] Tconstr(p, args, _) =>
    let declared =
      switch (project_type(ctx, u, ~from, p)) {
      | Some(key) =>
        touch_alias(ctx, key);
        [key];
      | None => []
      };
    (None, dedup(declared @ nodes(args)));
  | _ => (None, [])
  };
}

/* The node a type expression stands for.
   - a project type (bare, or applied to type variables only): its Alias
   - a built-in without arguments (int, string, exn, ...): a Builtin
   - an outside type without arguments (Buffer.t): a Ghost
   - anything else (tuple, arrow, variable, [shape list], [int tree],
     [(shape, string) result]): a Derived node keyed by its printed form,
     with the nodes of its own components as parts (see [formed]). */
and node_of_type =
    (ctx: ctx, u: unit_info, ~from: list(string), ty: Types.type_expr)
    : string =>
  switch (Types.get_desc(ty)) {
  | [@implicit_arity] Tpoly(t, _) => node_of_type(ctx, u, ~from, t)
  | [@implicit_arity] Tconstr(p, args, _) =>
    switch (project_type(ctx, u, ~from, p)) {
    | Some(key) when List.for_all(is_variable, args) =>
      touch_alias(ctx, key);
      key;
    | Some(_) => derived(ctx, u, ~from, ty)
    | None =>
      if (args != []) {
        derived(ctx, u, ~from, ty);
      } else if (is_predef(p)) {
        builtin(ctx, Path.name(p));
      } else {
        ghost(ctx, outside_name(p));
      }
    }
  | _ => derived(ctx, u, ~from, ty)
  }

and derived =
    (ctx: ctx, u: unit_info, ~from: list(string), ty: Types.type_expr)
    : string => {
  let key = print_type(ctx, u, ~qualified=true, ~from, ty);
  if (!Hashtbl.mem(ctx.seen, key)) {
    let (former, parts) = formed(ctx, u, ~from, ty);
    let label = print_type(ctx, u, ~qualified=false, ~from, ty);
    add_node(
      ctx,
      plain_node(~former, ~key, ~label, ~kind=Derived, ~parts, ()),
    );
  };
  key;
};

/* The source of a function of several arguments: one Product node per
   distinct list of argument nodes. [labels] are the arguments as written. */
let product =
    (ctx: ctx, arg_keys: list(string), ~labels: list(string)): string => {
  let key = "(" ++ String.concat(", ", arg_keys) ++ ")";
  if (!Hashtbl.mem(ctx.seen, key)) {
    let label = "(" ++ String.concat(", ", labels) ++ ")";
    add_node(
      ctx,
      plain_node(~key, ~label, ~kind=Product, ~parts=arg_keys, ()),
    );
  };
  key;
};

/* ---------- expressions: mentions, holes, tests ---------- */

/* Qualified names of the project's top-level values an expression refers to. */
let mentions = (ctx: ctx, u: unit_info, e: expression): list(string) => {
  let found = ref([]);
  let expr = (it: Tast_iterator.iterator, e: expression) => {
    switch (e.exp_desc) {
    | [@implicit_arity] Texp_ident(p, _, _) =>
      switch (project_value(ctx, u, p)) {
      | Some(name) => found := [name, ...found^]
      | None => ()
      }
    | _ => ()
    };
    Tast_iterator.default_iterator.expr(it, e);
  };

  let it = {
    ...Tast_iterator.default_iterator,
    expr,
  };
  it.expr(it, e);
  dedup(List.rev(found^));
};

let is_unit_value = (e: expression): bool =>
  switch (e.exp_desc) {
  | [@implicit_arity] Texp_construct(_, {cstr_name: "()", _}, []) => true
  | _ => false
  };

/* Is the body of a definition a placeholder? The body is what is left
   after the parameters: [fun x y -> BODY] or [function _ -> BODY]. */
let rec is_hole = (e: expression): bool =>
  switch (e.exp_desc) {
  | [@implicit_arity] Texp_function(_, Tfunction_body(body)) =>
    is_hole(body)
  | [@implicit_arity] Texp_function(_, Tfunction_cases({cases: [c], _})) =>
    is_hole(c.c_rhs)
  | [@implicit_arity]
    Texp_assert(
      {
        exp_desc:
          [@implicit_arity] Texp_construct(_, {cstr_name: "false", _}, []),
        _,
      },
      _,
    ) =>
    true
  | [@implicit_arity]
    Texp_apply(
      {exp_desc: [@implicit_arity] Texp_ident(p, _, _), _},
      [(_, Some(arg))],
    ) =>
    switch (outside_name(p), arg.exp_desc) {
    | ("failwith", Texp_constant([@implicit_arity] Const_string(s, _, _))) =>
      let s = String.lowercase_ascii(s);
      contains(~sub="todo", s) || contains(~sub="not implemented", s);
    | ("Obj.magic", _) => is_unit_value(arg)
    | _ => false
    }
  | _ => false
  };

/* ppx_inline_test turns [let%test "name" = e] into

     let () = Ppx_inline_test_lib.test ~config ~descr:(lazy "name") ~tags
                ~filename ~line_number ~start_pos ~end_pos (fun () -> e)

   ([test_unit] for [let%test_unit]; older versions pass [~descr] as a
   plain string, or go through [Ppx_inline_test_lib.Runtime]). Returns the
   description and the closure. */
let inline_test = (e: expression): option((string, expression)) => {
  let rec string_of = (e: expression) =>
    switch (e.exp_desc) {
    | Texp_constant([@implicit_arity] Const_string(s, _, _)) => Some(s)
    | Texp_lazy(e) => string_of(e)
    | _ => None
    };

  switch (e.exp_desc) {
  | [@implicit_arity]
    Texp_apply({exp_desc: [@implicit_arity] Texp_ident(p, _, _), _}, args)
      when
        String.starts_with(~prefix="Ppx_inline_test_lib", Path.name(p))
        && List.mem(Path.last(p), ["test", "test_unit"]) =>
    let descr =
      List.find_map(
        fun
        | (Asttypes.Labelled("descr"), Some(d)) => string_of(d)
        | _ => None,
        args,
      );

    let closure =
      List.find_map(
        fun
        | (Asttypes.Nolabel, Some(c)) => Some(c)
        | _ => None,
        List.rev(args),
      );

    Option.map(c => (Option.value(descr, ~default=""), c), closure);
  | _ => None
  };
};

let status = (ctx: ctx, file: string, line: int): G.test_status =>
  switch (ctx.failing) {
  | None => Unknown
  | Some(l) =>
    if (List.mem((file, line), l)) {
      Fail;
    } else {
      Pass;
    }
  };

let add_test =
    (
      ctx: ctx,
      u: unit_info,
      ~mpath: list(string),
      ~name: string,
      ~path_name: string,
      ~loc: Location.t,
      body: expression,
    )
    : unit => {
  let t_at = handle(u, mpath, path_name, loc);
  let test: G.test = (
    {
      t_name: name,
      t_status: status(ctx, u.u_file, t_at.line),
      t_refs: [],
      t_at,
    }: G.test
  );

  ctx.tests = [
    {
      ft_test: test,
      ft_uses: mentions(ctx, u, body),
      ft_alcotest: None,
    },
    ...ctx.tests,
  ];
};

/* Record every inline test in an expression. They sit at the top of a
   [let () = ...], or deeper inside a [let%test_module]. */
let find_inline_tests =
    (ctx: ctx, u: unit_info, ~mpath: list(string), e: expression): unit => {
  let expr = (it: Tast_iterator.iterator, e: expression) =>
    switch (inline_test(e)) {
    | Some((descr, closure)) =>
      let name =
        if (descr == "") {
          Printf.sprintf("line %d", e.exp_loc.loc_start.pos_lnum);
        } else {
          descr;
        };

      add_test(
        ctx,
        u,
        ~mpath,
        ~name,
        ~path_name=name,
        ~loc=e.exp_loc,
        closure,
      );
    | None => Tast_iterator.default_iterator.expr(it, e)
    };

  let it = {
    ...Tast_iterator.default_iterator,
    expr,
  };
  it.expr(it, e);
};

/* An Alcotest case, [Alcotest.test_case "name" `Quick f], or the same
   through a wrapper of one's own ([case("name", f)]): an application
   that makes a test case from a literal name and, last, a function.
   Returns the name and the function. */
let alcotest_case = (e: expression): option((string, expression)) => {
  let is_case = (ty: Types.type_expr) =>
    switch (Types.get_desc(ty)) {
    | [@implicit_arity] Tconstr(p, _, _) =>
      Path.last(p) == "test_case" && contains(~sub="Alcotest", Path.name(p))
    | _ => false
    };
  switch (e.exp_desc) {
  | [@implicit_arity] Texp_apply(_, args) when is_case(e.exp_type) =>
    let plain =
      List.filter_map(
        fun
        | (Asttypes.Nolabel, Some(arg)) => Some(arg)
        | _ => None,
        args,
      );
    let name =
      List.find_map(
        (arg: expression) =>
          switch (arg.exp_desc) {
          | Texp_constant([@implicit_arity] Const_string(s, _, _)) =>
            Some(s)
          | _ => None
          },
        plain,
      );
    switch (name, List.rev(plain)) {
    | (Some(name), [body, _, ..._]) => Some((name, body))
    | _ => None
    };
  | _ => None
  };
};

/* [a; b; c] as written, [None] for a list that is computed. */
let rec list_literal = (e: expression): option(list(expression)) =>
  switch (e.exp_desc) {
  | [@implicit_arity] Texp_construct(_, {cstr_name: "::", _}, [head, tail]) =>
    Option.map(rest => [head, ...rest], list_literal(tail))
  | [@implicit_arity] Texp_construct(_, {cstr_name: "[]", _}, []) =>
    Some([])
  | _ => None
  };

/* Record every Alcotest case in an expression. Its status, and where it
   is when its body is a function defined elsewhere in the file, are
   filled in by [assemble]. */
let find_alcotest_cases =
    (ctx: ctx, u: unit_info, ~mpath: list(string), e: expression): unit => {
  let add = (~suite: option((string, int)), ~loc, name, body: expression) => {
    let fn =
      switch (body.exp_desc) {
      | [@implicit_arity] Texp_ident(p, _, _) => project_value(ctx, u, p)
      | _ => None
      };
    let name =
      switch (suite) {
      | Some((suite, _)) => suite ++ ": " ++ name
      | None => name
      };
    ctx.tests = [
      {
        ft_test: {
          t_name: name,
          t_status: Unknown,
          t_refs: [],
          t_at: handle(u, mpath, name, loc),
        },
        ft_uses: mentions(ctx, u, body),
        ft_alcotest:
          Some({
            a_helpers: String.concat(".", u.u_mpath) ++ ".",
            a_case: suite,
            a_fn: fn,
          }),
      },
      ...ctx.tests,
    ];
  };
  let expr = (it: Tast_iterator.iterator, e: expression) =>
    switch (alcotest_case(e), e.exp_desc) {
    | (Some((name, body)), _) =>
      add(~suite=None, ~loc=e.exp_loc, name, body)
    /* ("suite", [case; case]) */
    | (
        None,
        Texp_tuple([
          {
            exp_desc:
              Texp_constant([@implicit_arity] Const_string(suite, _, _)),
            _,
          },
          cases,
        ]),
      )
        when list_literal(cases) != None =>
      List.iteri(
        (i, case: expression) =>
          switch (alcotest_case(case)) {
          | Some((name, body)) =>
            add(~suite=Some((suite, i)), ~loc=case.exp_loc, name, body)
          | None => it.expr(it, case)
          },
        Option.get(list_literal(cases)),
      )
    | (None, _) => Tast_iterator.default_iterator.expr(it, e)
    };

  let it = {
    ...Tast_iterator.default_iterator,
    expr,
  };
  it.expr(it, e);
};

/* ---------- pass 2: emit ---------- */

let is_unit_type = (ty: Types.type_expr): bool =>
  switch (Types.get_desc(ty)) {
  | [@implicit_arity] Tconstr(p, [], _) => Path.same(p, Predef.path_unit)
  | _ => false
  };

/* One top-level name: a test ([let test_xxx () = ...]), an edge (arrow
   type) or a value. [body] is [None] for an external. */
let define =
    (
      ctx: ctx,
      u: unit_info,
      scope: scope,
      ~name: string,
      ~ty: Types.type_expr,
      ~loc: Location.t,
      ~attrs: attributes,
      ~body: option(expression),
    )
    : unit => {
  let from = scope.mpath;
  let qname = qualify(from, name);
  let at = handle(u, from, name, loc);
  let print = print_type(ctx, u, ~qualified=false, ~from);
  /* [let f : 'a. ...] is stored under a quantifier: look through it. */
  let ty =
    switch (Types.get_desc(ty)) {
    | [@implicit_arity] Tpoly(t, _) => t
    | _ => ty
    };
  let prefix = "test_";
  switch (flatten_arrow(ty), body) {
  | (([arg], _), Some(body))
      when
        String.starts_with(~prefix, name)
        && String.length(name) > String.length(prefix)
        && is_unit_type(arg) =>
    let short =
      String.sub(
        name,
        String.length(prefix),
        String.length(name) - String.length(prefix),
      );

    add_test(ctx, u, ~mpath=from, ~name=short, ~path_name=name, ~loc, body);
  | (([], _), _) =>
    let value: G.value = (
      {
        v_name: qname,
        v_label: name,
        v_m_path: from,
        v_key: node_of_type(ctx, u, ~from, ty),
        v_ty: print(ty),
        v_doc:
          first_some(
            doc_of(attrs),
            sig_doc(scope.vis, ~is_type=false, name),
          ),
        v_err: false,
        v_deps: [],
        v_exported: value_exported(scope.vis, name),
        v_at: at,
      }: G.value
    );

    let uses =
      switch (body) {
      | Some(e) => mentions(ctx, u, e)
      | None => []
      };
    ctx.defs = [[@implicit_arity] Value(value, uses), ...ctx.defs];
  | ((args, result), _) =>
    let arg_keys = List.map(node_of_type(ctx, u, ~from), args);
    let src =
      switch (arg_keys) {
      | [key] => key
      | keys => product(ctx, keys, ~labels=List.map(print, args))
      };

    let dst = node_of_type(ctx, u, ~from, result);
    let edge: G.edge = (
      {
        e_name: qname,
        e_label: name,
        e_m_path: from,
        e_ty: print(ty),
        src,
        dst,
        e_doc:
          first_some(
            doc_of(attrs),
            sig_doc(scope.vis, ~is_type=false, name),
          ),
        e_err: false,
        e_hole:
          switch (body) {
          | Some(e) => is_hole(e)
          | None => false
          },
        main: false,
        e_deps: [],
        e_exported: value_exported(scope.vis, name),
        tests: [],
        e_at: at,
      }: G.edge
    );

    let uses =
      switch (body) {
      | Some(e) => mentions(ctx, u, e)
      | None => []
      };
    ctx.defs = [[@implicit_arity] Edge(edge, uses), ...ctx.defs];
  };
};

let emit = (ctx: ctx, u: unit_info, item: item): unit =>
  switch (item) {
  | [@implicit_arity] Type_item(scope, d) =>
    touch_alias(ctx, qualify(scope.mpath, d.typ_name.txt))
  | [@implicit_arity] Prim_item(scope, vd) =>
    define(
      ctx,
      u,
      scope,
      ~name=vd.val_name.txt,
      ~ty=vd.val_val.val_type,
      ~loc=vd.val_loc,
      ~attrs=vd.val_attributes,
      ~body=None,
    )
  | [@implicit_arity] Value_item(scope, vb, loc) =>
    switch (pat_bound_idents_full(vb.vb_pat)) {
    | [] =>
      /* [let () = ...] or [let _ = ...]: only of interest for its tests */
      find_inline_tests(ctx, u, ~mpath=scope.mpath, vb.vb_expr);
      find_alcotest_cases(ctx, u, ~mpath=scope.mpath, vb.vb_expr);
    | bound =>
      /* [let suite = [test_case ...]] */
      find_alcotest_cases(ctx, u, ~mpath=scope.mpath, vb.vb_expr);
      List.iter(
        ((_, name: Location.loc(string), ty, _)) =>
          define(
            ctx,
            u,
            scope,
            ~name=name.txt,
            ~ty,
            ~loc,
            ~attrs=vb.vb_attributes,
            ~body=Some(vb.vb_expr),
          ),
        bound,
      );
    }
  };

/* Fill in what needs the whole project: deps, tests per edge, [main]. */
let assemble = (ctx: ctx): G.t => {
  let def_name =
    fun
    | [@implicit_arity] Edge(e, _)
    | Carried_edge(e) => e.e_name
    | [@implicit_arity] Value(v, _)
    | Carried_value(v) => v.v_name;
  let uses_of =
    fun
    | [@implicit_arity] Edge(_, uses)
    | [@implicit_arity] Value(_, uses) => uses
    | Carried_edge(e) => e.e_deps
    | Carried_value(v) => v.v_deps;
  /* A name bound twice ([let x = 1 let x = x + 1]): the last one stands.
     [ctx.defs] is reversed, so the last binding of a name comes first. */
  let kept = Hashtbl.create(64);
  let defs =
    List.fold_left(
      (acc, d) =>
        if (Hashtbl.mem(kept, def_name(d))) {
          acc;
        } else {
          Hashtbl.replace(
            kept,
            def_name(d),
            switch (d) {
            | Edge(_)
            | Carried_edge(_) => true
            | Value(_)
            | Carried_value(_) => false
            },
          );
          [d, ...acc];
        },
      [],
      ctx.defs,
    );

  let is_def = name => Hashtbl.mem(kept, name);
  let is_edge = name => Hashtbl.find_opt(kept, name) == Some(true);
  let uses_by_name = Hashtbl.create(64)
  and edge_handles = Hashtbl.create(64);
  List.iter(
    d => {
      Hashtbl.replace(uses_by_name, def_name(d), uses_of(d));
      switch (d) {
      | [@implicit_arity] Edge(e, _)
      | Carried_edge(e) => Hashtbl.replace(edge_handles, e.e_name, e.e_at)
      | Value(_)
      | Carried_value(_) => ()
      };
    },
    defs,
  );
  /* A function of a test file that is the body of an Alcotest case is
     that test, not a function of the project: it is not an edge. */
  let test_fn = (t: found_test): option(string) =>
    switch (t.ft_alcotest) {
    | Some({a_fn: Some(f), a_helpers, _})
        when
          String.starts_with(~prefix=a_helpers, f)
          && Hashtbl.mem(edge_handles, f) =>
      Some(f)
    | _ => None
    };
  List.iter(t => Option.iter(Hashtbl.remove(kept), test_fn(t)), ctx.tests);
  let defs = List.filter(d => is_def(def_name(d)), defs);
  let uses_of_name = name =>
    Option.value(Hashtbl.find_opt(uses_by_name, name), ~default=[]);
  /* What an Alcotest case exercises: the project's functions outside
     its own file that its body mentions, or that the helpers it
     mentions in its file do, however many helpers deep. A case that
     reaches none is attached to the functions of its file it mentions. */
  let exercised = (a: alcotest, t: found_test): list(string) => {
    let seen = Hashtbl.create(16);
    let found = ref([]);
    let rec visit = name =>
      if (!Hashtbl.mem(seen, name)) {
        Hashtbl.replace(seen, name, ());
        if (String.starts_with(~prefix=a.a_helpers, name)) {
          List.iter(visit, uses_of_name(name));
        } else if (is_edge(name)) {
          found := [name, ...found^];
        };
      };
    List.iter(visit, t.ft_uses);
    switch (List.rev(found^), test_fn(t)) {
    | ([], Some(f)) => List.filter(is_edge, uses_of_name(f))
    | ([], None) => List.filter(is_edge, t.ft_uses)
    | (outside, _) => outside
    };
  };
  /* Failed if the test run named it (see extract.rei), or reported a
     location inside it. */
  let cases =
    List.filter_map(
      (t: found_test) =>
        Option.bind(t.ft_alcotest, a =>
          Option.map(((suite, i)) => ("alcotest:" ++ suite, i), a.a_case)
        ),
      ctx.tests,
    );
  let alcotest_status = (a: alcotest, at: G.handle): G.test_status =>
    switch (ctx.failing) {
    | None => Unknown
    | Some(l) =>
      let named =
        switch (a.a_case) {
        | Some((suite, i)) => List.mem(("alcotest:" ++ suite, i), l)
        | None => false
        };
      /* a case named as failing that is none of the cases whose suite
         is known: it may be this one */
      let unclaimed =
        a.a_case == None
        && List.exists(
             ((f, _) as failed) =>
               String.starts_with(~prefix="alcotest:", f)
               && !List.mem(failed, cases),
             l,
           );
      let located =
        List.exists(
          ((file, line)) =>
            file == at.file && at.line <= line && line <= at.end_line,
          l,
        );
      if (named || located) {
        Fail;
      } else if (unclaimed) {
        Unknown;
      } else {
        Pass;
      };
    };
  let tests =
    List.rev_map(
      (t: found_test) =>
        switch (t.ft_alcotest) {
        | None => {
            ...t.ft_test,
            t_refs: List.filter(is_edge, t.ft_uses),
          }
        | Some(a) =>
          let t_at =
            switch (Option.map(Hashtbl.find(edge_handles), test_fn(t))) {
            | Some(at) => at
            | None => t.ft_test.t_at
            };
          {
            ...t.ft_test,
            t_refs: exercised(a, t),
            t_status: alcotest_status(a, t_at),
            t_at,
          };
        },
      ctx.tests,
    );
  /* In the order they are in their file: a case found where it is run
     may have been placed at its function, further up. The tests of a
     file follow one another. */
  let tests = {
    let rec by_file = (l: list(G.test)) =>
      switch (l) {
      | [] => []
      | [t, ..._] =>
        let here = (o: G.test) => o.t_at.file == t.t_at.file;
        let rec split = (same, l) =>
          switch (l) {
          | [o, ...rest] when here(o) => split([o, ...same], rest)
          | _ => (List.rev(same), l)
          };
        let (same, rest) = split([], l);
        List.stable_sort(
          (a: G.test, b: G.test) => compare(a.t_at.line, b.t_at.line),
          same,
        )
        @ by_file(rest);
      };
    by_file(tests);
  };

  /* For each name: how many other definitions and tests mention it, and
     which tests do. */
  let users = Hashtbl.create(64)
  and tests_of = Hashtbl.create(64);
  let count = name =>
    Hashtbl.replace(
      users,
      name,
      1 + Option.value(Hashtbl.find_opt(users, name), ~default=0),
    );

  List.iter(
    d =>
      List.iter(
        n =>
          if (n != def_name(d)) {
            count(n);
          },
        uses_of(d),
      ),
    defs,
  );
  List.iter(
    (t: G.test) =>
      List.iter(
        n => {
          count(n);
          Hashtbl.replace(
            tests_of,
            n,
            [
              t,
              ...Option.value(Hashtbl.find_opt(tests_of, n), ~default=[]),
            ],
          );
        },
        t.t_refs,
      ),
    tests,
  );
  let tests_of = name =>
    List.rev(Option.value(Hashtbl.find_opt(tests_of, name), ~default=[]));
  let deps = (name, uses) => List.filter(n => n != name && is_def(n), uses);
  let edges =
    List.filter_map(
      fun
      | Value(_)
      | Carried_value(_) => None
      | [@implicit_arity] Edge(e, uses) =>
        Some({
          ...e,
          e_deps: deps(e.e_name, uses),
          main:
            Option.value(Hashtbl.find_opt(users, e.e_name), ~default=0) >= 2,
          tests: tests_of(e.e_name),
        })
      /* [main] and the deps stay as they were, less what is gone */
      | Carried_edge(e) =>
        Some({
          ...e,
          e_deps: deps(e.e_name, e.e_deps),
          tests: tests_of(e.e_name),
        }),
      defs,
    );

  {
    nodes: List.rev(ctx.nodes),
    edges,
    values:
      List.filter_map(
        fun
        | [@implicit_arity] Value(v, uses) =>
          Some({
            ...v,
            v_deps: deps(v.v_name, uses),
          })
        | Carried_value(v) =>
          Some({
            ...v,
            v_deps: deps(v.v_name, v.v_deps),
          })
        | Edge(_)
        | Carried_edge(_) => None,
        defs,
      ),
    loose_tests: List.filter((t: G.test) => t.t_refs == [], tests),
  };
};

/* ---------- reading the build directory ---------- */

/* dune preprocesses lib/scene.ml into lib/scene.pp.ml and compiles that;
   a Reason source lib/scene.re is compiled as lib/scene.re.ml. */
let strip_pp = (file: string): string => {
  let swap = ((suffix, by)) =>
    if (Filename.check_suffix(file, suffix)) {
      Some(Filename.chop_suffix(file, suffix) ++ by);
    } else {
      None;
    };

  List.find_map(
    swap,
    [
      (".re.pp.ml", ".re"),
      (".re.ml", ".re"),
      (".pp.ml", ".ml"),
      (".rei.pp.mli", ".rei"),
      (".rei.mli", ".rei"),
      (".pp.mli", ".mli"),
    ],
  )
  |> Option.value(~default=file);
};

let in_project = (~root: string, f: string): bool =>
  Filename.is_relative(f)
  && Sys.file_exists(root /\/ f)
  && !Sys.is_directory(root /\/ f);

/* The source file of an implementation, relative to the root: the file the
   compiler was given, or else the file the first located item points to
   (a Reason source, say). [None] if neither is in the project, as for the
   files dune generates. */
let source_file =
    (~root: string, cmt: Cmt_format.cmt_infos, str: structure)
    : option(string) => {
  let from_items =
    List.find_map(
      (si: structure_item) => {
        let f = si.str_loc.loc_start.pos_fname;
        if (f == "" || f == "_none_") {
          None;
        } else {
          Some(f);
        };
      },
      str.str_items,
    );

  List.find_opt(
    in_project(~root),
    Option.to_list(Option.map(strip_pp, cmt.cmt_sourcefile))
    @ Option.to_list(from_items),
  );
};

/* dune writes an alias module per wrapped library (source: vector.ml-gen)
   holding [module Scene = Vector__Scene]. Not part of the graph, but it
   says what [Vector.Scene] means. */
let read_alias_module = (ctx: ctx, modname: string, str: structure): unit =>
  List.iter(
    (si: structure_item) =>
      switch (si.str_desc) {
      | Tstr_module({
          mb_name: {txt: Some(sub), _},
          mb_expr: {mod_desc: [@implicit_arity] Tmod_ident(p, _), _},
          _,
        }) =>
        Hashtbl.replace(ctx.wrappers, (modname, sub), Path.name(p))
      | _ => ()
      },
    str.str_items,
  );

/* The signature of the .cmti next to a .cmt, when the module has an .mli. */
let interface_of = (cmt_path: string): vis => {
  let cmti = cmt_path ++ "i";
  if (!Sys.file_exists(cmti)) {
    All;
  } else {
    switch (Cmt_format.read_cmt(cmti).cmt_annots) {
    | Interface(sg) => Only(sg.sig_type)
    | _ => All
    | exception _ => All
    };
  };
};

/* A module whose source has an interface, but whose interface could not
   be read: it did not compile. What the module exports is then unknown. */
let interface_lost = (~root: string, ~file: string, vis: vis): bool =>
  switch (vis) {
  | All => in_project(~root, file ++ "i")
  | Only(_)
  | Nothing => false
  };

/* Names shown for the units: "Scene" for Vector__Scene. When two units
   would get the same name, fall back to "Vector.Scene", then to the short
   name tagged with its directory. [also] are the compiled names of units
   that are not being read but keep their names (the carried ones). */
let name_units =
    (~also: list(string), found: list((string, string, structure, vis)))
    : list((list(string), (string, string, structure, vis))) => {
  let short = modname =>
    switch (List.rev(split_double_underscore(modname))) {
    | [last, ..._] => [last]
    | [] => [modname]
    };

  let dotted = split_double_underscore;
  let modnames = List.map(((modname, _, _, _)) => modname, found) @ also;
  /* how many units each candidate name would be given to */
  let tally = name_of => {
    let t = Hashtbl.create(16);
    List.iter(
      x =>
        Hashtbl.replace(
          t,
          name_of(x),
          1 + Option.value(Hashtbl.find_opt(t, name_of(x)), ~default=0),
        ),
      modnames,
    );
    x => Hashtbl.find(t, name_of(x)) == 1;
  };

  let unique_short = tally(short)
  and unique_dotted = tally(dotted);
  List.map(
    ((modname, file, _, _) as x) => {
      let mpath =
        if (unique_short(modname)) {
          short(modname);
        } else if (unique_dotted(modname)) {
          dotted(modname);
        } else {
          [
            String.concat("", short(modname))
            ++ "@"
            ++ Filename.dirname(file),
          ];
        };

      (mpath, x);
    },
    found,
  );
};

/* ---------- carrying over ---------- */

/* While a file does not type-check it has no usable annotations, and its
   definitions would drop out of the graph until it builds again. Given
   the graph of an earlier extraction, such a file's nodes, edges, values
   and tests are taken from there instead, moved to where Locate finds
   them in the source as it stands. */

/* What an earlier graph says about one file. */
type carried = {
  c_file: string,
  c_prefix: list(string), /* the file's module, as shown: ["Geo"] */
  c_nodes: list(G.node),
  c_edges: list(G.edge),
  c_values: list(G.value),
  c_tests: list(G.test),
};

/* Every test of a graph, once, in order. */
let all_tests = (g: G.t): list(G.test) => {
  let seen = Hashtbl.create(16);
  List.concat_map((e: G.edge) => e.tests, g.edges)
  @ g.loose_tests
  |> List.filter((t: G.test) => {
       let first = !Hashtbl.mem(seen, (t.t_at, t.t_name));
       Hashtbl.replace(seen, (t.t_at, t.t_name), ());
       first;
     });
};

/* The files a graph's definitions are in. */
let files_of = (g: G.t): list(string) =>
  List.filter_map(
    (n: G.node) => Option.map((h: G.handle) => h.file, n.at),
    g.nodes,
  )
  @ List.map((e: G.edge) => e.e_at.file, g.edges)
  @ List.map((v: G.value) => v.v_at.file, g.values)
  @ List.map((t: G.test) => t.t_at.file, all_tests(g))
  |> List.sort_uniq(compare);

/* "lib/geo.ml" -> "Geo" */
let module_of_file = (file: string): string =>
  String.capitalize_ascii(
    Filename.remove_extension(Filename.basename(file)),
  );

/* The name a file's module was shown under: the start of its definitions'
   module paths, up to the file's own module (see [name_units]). */
let prefix_of = (file: string, m_paths: list(list(string))): list(string) => {
  let own = module_of_file(file);
  let is_own = part =>
    part == own || String.starts_with(~prefix=own ++ "@", part);
  let rec upto =
    fun
    | [] => None
    | [part, ..._] when is_own(part) => Some([part])
    | [part, ...rest] => Option.map(p => [part, ...p], upto(rest));
  switch (List.find_map(upto, m_paths)) {
  | Some(prefix) => prefix
  | None => [own]
  };
};

/* A handle moved to where its item is now. [None]: the source parses and
   has no such item any more. */
let relocate =
    (located: option(Locate.t), kinds: list(Locate.kind), h: G.handle)
    : option(G.handle) =>
  switch (located) {
  | None => Some(h)
  | Some(l) =>
    switch (List.find_map(kind => Locate.find(l, kind, h.path), kinds)) {
    | Some((line, end_line)) =>
      Some({
        ...h,
        line,
        end_line,
      })
    | None =>
      if (l.exact) {
        None;
      } else {
        Some(h);
      }
    }
  };

let carry =
    (~root: string, ~build: string, previous: G.t, file: string): carried => {
  let here = (h: G.handle) => h.file == file;
  let nodes =
    List.filter(
      (n: G.node) =>
        switch (n.at) {
        | Some(h) => here(h)
        | None => false
        },
      previous.nodes,
    );
  let edges = List.filter((e: G.edge) => here(e.e_at), previous.edges);
  let values = List.filter((v: G.value) => here(v.v_at), previous.values);
  let tests =
    List.filter((t: G.test) => here(t.t_at), all_tests(previous));
  let prefix =
    prefix_of(
      file,
      List.map((n: G.node) => n.m_path, nodes)
      @ List.map((e: G.edge) => e.e_m_path, edges)
      @ List.map((v: G.value) => v.v_m_path, values)
      @ List.map(
          (t: G.test) => String.split_on_char('/', t.t_at.path),
          tests,
        ),
    );
  let located = Locate.source(~root, ~build, ~prefix, file);
  let by_line = (line_of, l) =>
    List.stable_sort((a, b) => compare(line_of(a), line_of(b)), l);
  let moved = (h: G.handle, (line, end_line)): G.handle => {
    ...h,
    line,
    end_line,
  };
  /* A value that is not written in the file was derived by a ppx from
     a type, and was placed where the type is: it follows the type. */
  let relocate_def = (h: G.handle): option(G.handle) =>
    switch (relocate(located, [Locate.Val], h), located) {
    | (Some(at), Some(l)) when Locate.find(l, Locate.Val, h.path) == None =>
      let around =
        List.find_map(
          (n: G.node) =>
            switch (n.at) {
            | Some(t) when t.line <= h.line && h.end_line <= t.end_line =>
              Some(Locate.find(l, Locate.Type, t.path))
            | _ => None
            },
          nodes,
        );
      switch (around) {
      | Some(Some(span)) => Some(moved(h, span))
      | Some(None) when l.exact => None /* its type is gone */
      | _ => Some(at)
      };
    | (at, _) => at
    };
  /* An inline test without a description is named by its text
     ("<<total empty = 0>>") or its line ("line 12"). When it is not
     found under that name it is looked for by its place among the
     tests without a description of its module, if there are still as
     many of them. */
  let parent = (path: string): string =>
    switch (String.rindex_opt(path, '/')) {
    | Some(i) => String.sub(path, 0, i)
    | None => ""
    };
  let by_line_number = (t: G.test) =>
    String.starts_with(~prefix="line ", t.t_name);
  let undescribed = (t: G.test) =>
    by_line_number(t) || String.starts_with(~prefix="<<", t.t_name);
  let place = (t: G.test): option((int, int)) => {
    let fellows =
      List.filter(
        (o: G.test) =>
          undescribed(o) && parent(o.t_at.path) == parent(t.t_at.path),
        tests,
      )
      |> by_line((o: G.test) => o.t_at.line);
    let find = k =>
      Option.bind(located, l =>
        Locate.find(
          l,
          Locate.Test,
          parent(t.t_at.path) ++ "/" ++ Locate.unnamed(k),
        )
      );
    let rec index = (k, l) =>
      switch (l) {
      | [] => None
      | [o, ..._] when o === t => Some(k)
      | [_, ...rest] => index(k + 1, rest)
      };
    if (undescribed(t)
        && find(List.length(fellows)) != None
        && find(List.length(fellows) + 1) == None) {
      Option.bind(index(1, fellows), find);
    } else {
      None;
    };
  };
  {
    c_file: file,
    c_prefix: prefix,
    c_nodes:
      List.filter_map(
        (n: G.node) =>
          Option.bind(n.at, relocate(located, [Locate.Type]))
          |> Option.map(at =>
               {
                 ...n,
                 n_err: false,
                 at: Some(at),
               }
             ),
        nodes,
      ),
    c_edges:
      List.filter_map(
        (e: G.edge) =>
          relocate_def(e.e_at)
          |> Option.map(e_at =>
               {
                 ...e,
                 e_err: false,
                 e_at,
               }
             ),
        edges,
      )
      |> by_line((e: G.edge) => e.e_at.line),
    c_values:
      List.filter_map(
        (v: G.value) =>
          relocate_def(v.v_at)
          |> Option.map(v_at =>
               {
                 ...v,
                 v_err: false,
                 v_at,
               }
             ),
        values,
      )
      |> by_line((v: G.value) => v.v_at.line),
    c_tests:
      List.filter_map(
        (t: G.test) => {
          /* [let test_x () = ...] and the function of an Alcotest case
             are found as values, an inline test by its description */
          let by_name =
            Option.bind(located, l =>
              List.find_map(
                kind => Locate.find(l, kind, t.t_at.path),
                [Locate.Test, Locate.Val],
              )
            );
          let t_at =
            switch (located, by_name, place(t)) {
            | (None, _, _) => Some(t.t_at)
            | (Some(_), Some(span), _)
            | (Some(_), None, Some(span)) => Some(moved(t.t_at, span))
            | (Some(l), None, None) =>
              if (l.exact && !by_line_number(t)) {
                None;
              } else {
                Some(t.t_at);
              }
            };
          Option.map(
            (t_at: G.handle) =>
              if (by_line_number(t)) {
                let t_name = Printf.sprintf("line %d", t_at.line);
                {
                  ...t,
                  t_name,
                  t_status: G.Unknown,
                  t_at: {
                    ...t_at,
                    path: parent(t_at.path) ++ "/" ++ t_name,
                  },
                };
              } else {
                {
                  ...t,
                  t_status: G.Unknown,
                  t_at,
                };
              },
            t_at,
          );
        },
        tests,
      )
      |> by_line((t: G.test) => t.t_at.line),
  };
};

/* The compiled names a carried file's module may be referred to by, when
   its annotations do not say: its own name, and that name behind one of
   dune's alias modules. */
let guess_modnames = (ctx: ctx, file: string): list(string) => {
  let own = module_of_file(file);
  let wrapped =
    Hashtbl.fold(
      ((_, sub), modname, acc) =>
        if (sub == own) {
          [modname, ...acc];
        } else {
          acc;
        },
      ctx.wrappers,
      [],
    );
  List.sort(compare, wrapped) @ [own];
};

let register_carried = (ctx: ctx, ~modnames: list(string), c: carried): unit => {
  List.iter(
    modname =>
      if (!Hashtbl.mem(ctx.units, modname)
          && !Hashtbl.mem(ctx.carried_units, modname)) {
        Hashtbl.replace(
          ctx.carried_units,
          modname,
          String.concat(".", c.c_prefix),
        );
      },
    modnames,
  );
  List.iter(
    (n: G.node) => {
      Hashtbl.replace(ctx.carried_types, n.key, ());
      Hashtbl.replace(ctx.previous_nodes, n.key, n);
    },
    c.c_nodes,
  );
  List.iter(
    (e: G.edge) => Hashtbl.replace(ctx.def_names, e.e_name, ()),
    c.c_edges,
  );
  List.iter(
    (v: G.value) => Hashtbl.replace(ctx.def_names, v.v_name, ()),
    c.c_values,
  );
};

/* A carried file's turn in pass 2: the nodes its definitions need, in
   the earlier graph's order, then the definitions and tests. */
let emit_carried = (ctx: ctx, previous: G.t, c: carried): unit => {
  let needed = Hashtbl.create(32);
  let rec need = key =>
    if (!Hashtbl.mem(needed, key)) {
      Hashtbl.replace(needed, key, ());
      switch (Hashtbl.find_opt(ctx.previous_nodes, key)) {
      | Some(n)
          when
            !Hashtbl.mem(ctx.type_decls, key)
            && (n.kind != Alias || Hashtbl.mem(ctx.carried_types, key)) =>
        List.iter(need, n.parts)
      | _ => ()
      };
    };
  let keys =
    List.map((n: G.node) => n.key, c.c_nodes)
    @ List.concat_map((e: G.edge) => [e.src, e.dst], c.c_edges)
    @ List.map((v: G.value) => v.v_key, c.c_values);
  List.iter(need, keys);
  List.iter(
    (n: G.node) =>
      if (Hashtbl.mem(needed, n.key)) {
        add_previous(ctx, n.key);
      },
    previous.nodes,
  );
  List.iter(add_previous(ctx), keys); /* any the earlier graph lacks */
  /* in source order, edges and values apart: only the order within each
     shows in the graph */
  List.iter(
    (e: G.edge) => ctx.defs = [Carried_edge(e), ...ctx.defs],
    c.c_edges,
  );
  List.iter(
    (v: G.value) => ctx.defs = [Carried_value(v), ...ctx.defs],
    c.c_values,
  );
  List.iter(
    (t: G.test) =>
      ctx.tests = [
        {
          ft_test: t,
          ft_uses: t.t_refs,
          ft_alcotest: None,
        },
        ...ctx.tests,
      ],
    c.c_tests,
  );
};

/* Are a module's annotations those of its source as it stands? dune
   leaves the old ones in place when a file stops parsing, or when a
   module it depends on stops compiling. The compiler records a digest of
   the file it was given; that file is [dir]'s copy of the source or one
   of its preprocessed forms, which dune removes when the source no
   longer parses. */
let up_to_date = (~dir: string, cmt: Cmt_format.cmt_infos): bool =>
  switch (cmt.cmt_sourcefile, cmt.cmt_source_digest) {
  | (Some(f), Some(digest)) =>
    let path =
      if (Filename.is_relative(f)) {
        dir /\/ f;
      } else {
        f;
      };
    switch (Digest.file(path)) {
    | d => d == digest
    | exception _ => false
    };
  | _ => true
  };

/* Set [err] on the definitions the compiler's errors fall in: by line
   for an error in an implementation; for one in an interface, the
   definition its [val] or [type] stands for. */
let mark_broken =
    (
      ~root: string,
      ~build: string,
      ~prefixes: list((string, list(string))),
      broken: list((string, int)),
      g: G.t,
    )
    : G.t =>
  if (broken == []) {
    g;
  } else {
    let by_line = (h: G.handle) =>
      List.exists(
        ((file, line)) =>
          file == h.file && h.line <= line && line <= h.end_line,
        broken,
      );
    /* (implementation file, kind, entry path) */
    let by_name =
      List.concat_map(
        ((file, line)) =>
          if (Filename.check_suffix(file, ".mli")
              || Filename.check_suffix(file, ".rei")) {
            let impl = String.sub(file, 0, String.length(file) - 1);
            switch (List.assoc_opt(impl, prefixes)) {
            | None => []
            | Some(prefix) =>
              switch (Locate.source(~root, ~build, ~prefix, file)) {
              | None => []
              | Some(located) =>
                List.map(
                  ((kind, path)) => (impl, kind, path),
                  Locate.containing(located, line),
                )
              }
            };
          } else {
            [];
          },
        broken,
      );
    let hit = (kind, h: G.handle) =>
      by_line(h) || List.mem((h.file, kind, h.path), by_name);
    {
      ...g,
      nodes:
        List.map(
          (n: G.node) =>
            switch (n.at) {
            | Some(h) when hit(Locate.Type, h) => {
                ...n,
                n_err: true,
              }
            | _ => n
            },
          g.nodes,
        ),
      edges:
        List.map(
          (e: G.edge) =>
            if (hit(Locate.Val, e.e_at)) {
              {
                ...e,
                e_err: true,
              };
            } else {
              e;
            },
          g.edges,
        ),
      values:
        List.map(
          (v: G.value) =>
            if (hit(Locate.Val, v.v_at)) {
              {
                ...v,
                v_err: true,
              };
            } else {
              v;
            },
          g.values,
        ),
    };
  };

/* ---------- the entry point ---------- */

/* One file's turn in pass 2. */
type turn =
  | Fresh(unit_info)
  | Carried(carried);

let extract =
    (
      ~root: string,
      ~build_dir: string,
      ~failing: option(list((string, int))),
      ~previous: option(G.t),
      ~broken: list((string, int)),
    )
    : result(G.t, string) => {
  let ctx = {
    failing,
    units: Hashtbl.create(16),
    wrappers: Hashtbl.create(16),
    mod_aliases: Hashtbl.create(16),
    type_decls: Hashtbl.create(64),
    def_names: Hashtbl.create(64),
    carried_units: Hashtbl.create(8),
    carried_types: Hashtbl.create(16),
    previous_nodes: Hashtbl.create(64),
    seen: Hashtbl.create(64),
    nodes: [],
    defs: [],
    tests: [],
  };

  let dir = root /\/ build_dir /\/ "default";
  let cmts =
    List.filter(f => Filename.check_suffix(f, ".cmt"), find_cmts(dir));
  let previous_files =
    switch (previous) {
    | Some(g) => files_of(g)
    | None => []
    };
  /* Read every implementation. A file that failed to type-check has only
     a partial tree: it is left out. So is one whose annotations are out
     of date or whose interface did not compile, when the earlier graph
     has it. [lost] remembers what such modules are compiled as. */
  let unreadable = ref(0);
  let lost = ref([]);
  let found =
    List.filter_map(
      path =>
        switch (Cmt_format.read_cmt(path)) {
        | exception _ =>
          incr(unreadable);
          None;
        | cmt =>
          let lose = file => lost := [(file, cmt.cmt_modname), ...lost^];
          switch (cmt.cmt_annots) {
          | Implementation(str) =>
            let generated =
              switch (cmt.cmt_sourcefile) {
              | Some(f) => Filename.check_suffix(f, "-gen")
              | None => false
              };

            if (generated) {
              read_alias_module(ctx, cmt.cmt_modname, str);
              None;
            } else {
              switch (source_file(~root, cmt, str)) {
              | Some(file) =>
                let vis = interface_of(path);
                if (List.mem(file, previous_files)
                    && (
                      !up_to_date(~dir, cmt)
                      || interface_lost(~root, ~file, vis)
                    )) {
                  lose(file);
                  None;
                } else {
                  Some((cmt.cmt_modname, file, str, vis));
                };
              | None => None
              };
            };
          | _ =>
            incr(unreadable);
            Option.map(strip_pp, cmt.cmt_sourcefile)
            |> Option.iter(file =>
                 if (in_project(~root, file)) {
                   lose(file);
                 }
               );
            None;
          };
        },
      cmts,
    );

  /* The same source can be compiled more than once: keep the first. */
  let sources = Hashtbl.create(16);
  let found =
    List.filter(
      ((_, file, _, _)) => {
        let first = !Hashtbl.mem(sources, file);
        Hashtbl.replace(sources, file, ());
        first;
      },
      found,
    );

  /* What is carried over: the earlier graph's files that were not read
     this time and still exist. */
  let carried =
    switch (previous) {
    | None => []
    | Some(g) =>
      List.filter(
        file => !Hashtbl.mem(sources, file) && in_project(~root, file),
        previous_files,
      )
      |> List.map(carry(~root, ~build=dir, g))
    };

  if (found == [] && carried == []) {
    Error(
      if (cmts == []) {
        Printf.sprintf(
          "no .cmt files under %s (build the project with: dune build --root . --build-dir %s @check)",
          dir,
          build_dir,
        );
      } else {
        Printf.sprintf(
          "no module under %s could be read (%d of %d .cmt files unreadable or not type-checked)",
          dir,
          unreadable^,
          List.length(cmts),
        );
      },
    );
  } else {
    let carried =
      List.map(
        c =>
          (
            switch (List.assoc_opt(c.c_file, lost^)) {
            | Some(modname) => [modname]
            | None => guess_modnames(ctx, c.c_file)
            },
            c,
          ),
        carried,
      );
    let units =
      name_units(
        ~also=List.map(((modnames, _)) => List.hd(modnames), carried),
        found,
      )
      |> List.map(((mpath, (modname, file, str, vis))) =>
           {
             u_modname: modname,
             u_mpath: mpath,
             u_file: file,
             u_str: str,
             u_vis: vis,
             u_types: Ident.Tbl.create(16),
             u_mods: Ident.Tbl.create(8),
             u_vals: Ident.Tbl.create(32),
             u_mtys: Ident.Tbl.create(8),
             u_items: [],
           }
         )
      |> List.sort((a, b) => compare(a.u_file, b.u_file));

    List.iter(
      u =>
        if (!Hashtbl.mem(ctx.units, u.u_modname)) {
          Hashtbl.replace(ctx.units, u.u_modname, u);
        },
      units,
    );
    Option.iter(
      (g: G.t) =>
        List.iter(
          (n: G.node) => Hashtbl.replace(ctx.previous_nodes, n.key, n),
          g.nodes,
        ),
      previous,
    );
    List.iter(
      ((modnames, c)) => register_carried(ctx, ~modnames, c),
      carried,
    );
    List.iter(
      u =>
        collect(
          ctx,
          u,
          {
            mpath: u.u_mpath,
            vis: u.u_vis,
          },
          u.u_str.str_items,
        ),
      units,
    );
    /* Pass 2, file by file in the order of their names, carried files
       taking their turn among the others. */
    let file_of =
      fun
      | Fresh(u) => u.u_file
      | Carried(c) => c.c_file;
    List.map(u => Fresh(u), units)
    @ List.map(((_, c)) => Carried(c), carried)
    |> List.stable_sort((a, b) => compare(file_of(a), file_of(b)))
    |> List.iter(
         fun
         | Fresh(u) => List.iter(emit(ctx, u), List.rev(u.u_items))
         | Carried(c) => emit_carried(ctx, Option.get(previous), c),
       );
    Ok(
      mark_broken(
        ~root,
        ~build=dir,
        ~prefixes=
          List.map(u => (u.u_file, u.u_mpath), units)
          @ List.map(((_, c)) => (c.c_file, c.c_prefix), carried),
        broken,
        assemble(ctx),
      ),
    );
  };
};
