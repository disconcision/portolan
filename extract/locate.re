/* Where the top-level items of a source file are now, by name.

   Used when a module cannot be type-checked: its definitions are carried
   over from an earlier graph, and this says where each one sits in the
   source as it stands. Three ways of finding out, best first:

   1. the parse tree dune's preprocessing left in the build directory
      (Reason sources: lib/a.re.ml; ppx: lib/a.pp.ml). It is there exactly
      when the current source parses;
   2. the compiler's parser on the source text (.ml and .mli);
   3. a scan of the text for binders, by indentation, when the file does
      not parse. This one is approximate. */

type kind =
  | Type
  | Val /* a let-bound name, an external, a [val] */
  | Test; /* an inline test, by its description */

type t = {
  /* From a parse tree: a name that is not listed is not in the file. */
  exact: bool,
  /* (kind, entry path such as "Geo/Angle/to_deg") -> (first, last line) */
  spans: Hashtbl.t((kind, string), (int, int)),
};

let find = (t: t, kind: kind, path: string): option((int, int)) =>
  Hashtbl.find_opt(t.spans, (kind, path));

/* The items whose span holds [line]. */
let containing = (t: t, line: int): list((kind, string)) =>
  Hashtbl.fold(
    (item, (first, last), acc) =>
      if (first <= line && line <= last) {
        [item, ...acc];
      } else {
        acc;
      },
    t.spans,
    [],
  )
  |> List.sort(compare);

let add =
    (
      spans,
      kind: kind,
      mpath: list(string),
      name: string,
      first: int,
      last: int,
    ) =>
  /* a name bound twice: the last one stands */
  Hashtbl.replace(
    spans,
    (kind, String.concat("/", mpath @ [name])),
    (first, last),
  );

/* ---------- from a parse tree ---------- */

open Parsetree;

let add_loc = (spans, kind, mpath, name, loc: Location.t) =>
  add(spans, kind, mpath, name, loc.loc_start.pos_lnum, loc.loc_end.pos_lnum);

/* An inline test without a description has nothing to be looked up by
   but its place among the others of its module: "#1", "#2", ... */
let unnamed = (n: int): string => "#" ++ string_of_int(n);

let add_unnamed_lines = (spans, mpath, first: int, last: int) => {
  let rec free = n =>
    if (Hashtbl.mem(
          spans,
          (Test, String.concat("/", mpath @ [unnamed(n)])),
        )) {
      free(n + 1);
    } else {
      n;
    };
  add(spans, Test, mpath, unnamed(free(1)), first, last);
};

let add_unnamed = (spans, mpath, loc: Location.t) =>
  add_unnamed_lines(
    spans,
    mpath,
    loc.loc_start.pos_lnum,
    loc.loc_end.pos_lnum,
  );

let pattern_names = (p: pattern): list(string) => {
  let found = ref([]);
  let pat = (it: Ast_iterator.iterator, p: pattern) => {
    switch (p.ppat_desc) {
    | Ppat_var(name)
    | [@implicit_arity] Ppat_alias(_, name) => found := [name.txt, ...found^]
    | _ => ()
    };
    Ast_iterator.default_iterator.pat(it, p);
  };

  let it = {
    ...Ast_iterator.default_iterator,
    pat,
  };
  it.pat(it, p);
  List.rev(found^);
};

let rec strip_constraint = (me: module_expr): module_expr =>
  switch (me.pmod_desc) {
  | [@implicit_arity] Pmod_constraint(inner, _) => strip_constraint(inner)
  | _ => me
  };

let string_constant = (e: expression): option(string) =>
  switch (e.pexp_desc) {
  | Pexp_constant([@implicit_arity] Pconst_string(s, _, _)) => Some(s)
  | Pexp_lazy({
      pexp_desc: Pexp_constant([@implicit_arity] Pconst_string(s, _, _)),
      _,
    }) =>
    Some(s)
  | _ => None
  };

let is_inline_test = (f: Longident.t): bool =>
  switch (Longident.flatten(f)) {
  | ["Ppx_inline_test_lib", ...rest] =>
    switch (List.rev(rest)) {
    | ["test" | "test_unit", ..._] => true
    | _ => false
    }
  | _ => false
  | exception _ => false
  };

/* Inline tests as ppx_inline_test leaves them (see Extract.inline_test):
   [Ppx_inline_test_lib.test ~descr:(lazy "name") ... (fun () -> e)]. */
let expanded_tests = (spans, mpath, e: expression) => {
  let expr = (it: Ast_iterator.iterator, e: expression) =>
    switch (e.pexp_desc) {
    | [@implicit_arity] Pexp_apply({pexp_desc: Pexp_ident(f), _}, args)
        when is_inline_test(f.txt) =>
      let descr =
        List.find_map(
          fun
          | (Asttypes.Labelled("descr"), d) => string_constant(d)
          | _ => None,
          args,
        );
      switch (descr) {
      | Some(name) when name != "" =>
        add_loc(spans, Test, mpath, name, e.pexp_loc)
      | _ => add_unnamed(spans, mpath, e.pexp_loc)
      };
    | _ => Ast_iterator.default_iterator.expr(it, e)
    };

  let it = {
    ...Ast_iterator.default_iterator,
    expr,
  };
  it.expr(it, e);
};

/* [within]: where the items are when they do not say (what a ppx wrote
   after a type, see Extract.collect). */
/* Alcotest cases whose body is written in place, under the names
   Extract gives them: [test_case "name" ...] as "name", and inside
   [("suite", [...])] any call with a literal name as "suite: name" (it
   may be a wrapper of [test_case]; without types one cannot tell, and a
   span nobody asks for does no harm). */
let alcotest_cases = (spans, mpath, e: expression) => {
  let name_of = (args): option(string) =>
    List.find_map(
      fun
      | (Asttypes.Nolabel, arg) => string_constant(arg)
      | _ => None,
      args,
    );
  let is_test_case = (f: Longident.t) =>
    switch (Longident.last(f)) {
    | "test_case" => true
    | _ => false
    | exception _ => false
    };
  let rec list_literal = (e: expression): option(list(expression)) =>
    switch (e.pexp_desc) {
    | [@implicit_arity]
      Pexp_construct(
        {txt: Lident("::"), _},
        Some({pexp_desc: Pexp_tuple([head, tail]), _}),
      ) =>
      Option.map(rest => [head, ...rest], list_literal(tail))
    | [@implicit_arity] Pexp_construct({txt: Lident("[]"), _}, None) =>
      Some([])
    | _ => None
    };
  let expr = (it: Ast_iterator.iterator, e: expression) =>
    switch (e.pexp_desc) {
    | [@implicit_arity] Pexp_apply({pexp_desc: Pexp_ident(f), _}, args)
        when is_test_case(f.txt) && name_of(args) != None =>
      add_loc(spans, Test, mpath, Option.get(name_of(args)), e.pexp_loc)
    | Pexp_tuple([suite, cases])
        when string_constant(suite) != None && list_literal(cases) != None =>
      List.iter(
        (case: expression) =>
          switch (case.pexp_desc) {
          | [@implicit_arity] Pexp_apply(_, args) when name_of(args) != None =>
            add_loc(
              spans,
              Test,
              mpath,
              Option.get(string_constant(suite))
              ++ ": "
              ++ Option.get(name_of(args)),
              case.pexp_loc,
            )
          | _ => it.expr(it, case)
          },
        Option.get(list_literal(cases)),
      )
    | _ => Ast_iterator.default_iterator.expr(it, e)
    };

  let it = {
    ...Ast_iterator.default_iterator,
    expr,
  };
  it.expr(it, e);
};

let rec structure =
        (
          ~within: option(Location.t)=?,
          spans,
          mpath: list(string),
          items: structure,
        )
        : unit => {
  let placed = (loc: Location.t): Location.t =>
    switch (within) {
    | Some(around) when loc.loc_start.pos_lnum == 0 => around
    | _ => loc
    };
  List.iter(
    (si: structure_item) =>
      switch (si.pstr_desc) {
      | [@implicit_arity] Pstr_type(_, decls) =>
        List.iter(
          (d: type_declaration) =>
            add_loc(spans, Type, mpath, d.ptype_name.txt, d.ptype_loc),
          decls,
        )
      | [@implicit_arity] Pstr_value(_, bindings) =>
        let several = List.length(bindings) > 1;
        List.iter(
          (vb: value_binding) => {
            let loc =
              if (several) {
                vb.pvb_loc;
              } else {
                si.pstr_loc;
              };
            switch (pattern_names(vb.pvb_pat)) {
            | [] =>
              expanded_tests(spans, mpath, vb.pvb_expr);
              alcotest_cases(spans, mpath, vb.pvb_expr);
            | names =>
              alcotest_cases(spans, mpath, vb.pvb_expr);
              List.iter(
                name => add_loc(spans, Val, mpath, name, placed(loc)),
                names,
              );
            };
          },
          bindings,
        );
      | Pstr_primitive(vd) =>
        add_loc(spans, Val, mpath, vd.pval_name.txt, vd.pval_loc)
      | Pstr_module(mb) => module_binding(spans, mpath, mb)
      | Pstr_recmodule(mbs) => List.iter(module_binding(spans, mpath), mbs)
      | Pstr_include({pincl_mod, _}) =>
        switch (strip_constraint(pincl_mod).pmod_desc) {
        | Pmod_structure(s) =>
          structure(~within=placed(si.pstr_loc), spans, mpath, s)
        | _ => ()
        }
      /* [let%test "name" = e], before ppx_inline_test has run */
      | [@implicit_arity]
        Pstr_extension(
          (
            {txt: ("test" | "test_unit" | "test_module") as ext, _},
            PStr([{pstr_desc: [@implicit_arity] Pstr_value(_, [vb]), _}]),
          ),
          _,
        ) =>
        switch (ext, vb.pvb_pat.ppat_desc, vb.pvb_expr.pexp_desc) {
        | ("test_module", _, Pexp_pack(me)) =>
          switch (strip_constraint(me).pmod_desc) {
          | Pmod_structure(s) => structure(spans, mpath, s)
          | _ => ()
          }
        | (
            "test" | "test_unit",
            Ppat_constant([@implicit_arity] Pconst_string(name, _, _)),
            _,
          ) =>
          add_loc(spans, Test, mpath, name, si.pstr_loc)
        | ("test" | "test_unit", _, _) =>
          add_unnamed(spans, mpath, si.pstr_loc)
        | _ => ()
        }
      | _ => ()
      },
    items,
  );
}

and module_binding = (spans, mpath, mb: module_binding): unit =>
  switch (mb.pmb_name.txt, strip_constraint(mb.pmb_expr).pmod_desc) {
  | (Some(name), Pmod_structure(s)) => structure(spans, mpath @ [name], s)
  | _ => ()
  };

let rec signature = (spans, mpath: list(string), items: signature): unit =>
  List.iter(
    (si: signature_item) =>
      switch (si.psig_desc) {
      | Psig_value(vd) =>
        add_loc(spans, Val, mpath, vd.pval_name.txt, vd.pval_loc)
      | [@implicit_arity] Psig_type(_, decls) =>
        List.iter(
          (d: type_declaration) =>
            add_loc(spans, Type, mpath, d.ptype_name.txt, d.ptype_loc),
          decls,
        )
      | Psig_module({
          pmd_name: {txt: Some(name), _},
          pmd_type: {pmty_desc: Pmty_signature(sg), _},
          _,
        }) =>
        signature(spans, mpath @ [name], sg)
      | _ => ()
      },
    items,
  );

/* ---------- getting a parse tree ---------- */

let read_file = (path: string): option(string) =>
  switch (open_in_bin(path)) {
  | exception (Sys_error(_)) => None
  | ic =>
    Fun.protect(
      ~finally=() => close_in_noerr(ic),
      () =>
        switch (really_input_string(ic, in_channel_length(ic))) {
        | s => Some(s)
        | exception _ => None
        },
    )
  };

/* A parse tree as dune's preprocessors write it: the compiler's magic
   number, the source file's name, the tree. The magic number is checked,
   so a tree of another compiler version is not read. */
let read_ast = (path: string, ~magic: string): option('ast) =>
  switch (open_in_bin(path)) {
  | exception (Sys_error(_)) => None
  | ic =>
    Fun.protect(
      ~finally=() => close_in_noerr(ic),
      () =>
        switch (really_input_string(ic, String.length(magic))) {
        | m when m == magic =>
          let _name: string = input_value(ic);
          Some(input_value(ic));
        | _ => None
        | exception _ => None
        },
    )
  };

type tree =
  | Impl(Parsetree.structure)
  | Intf(Parsetree.signature);

let parse_text = (~file: string, ~intf: bool, text: string): option(tree) => {
  let lexbuf = Lexing.from_string(text);
  Location.init(lexbuf, file);
  /* the lexer's warnings would go to stderr */
  ignore(Warnings.parse_options(false, "-a"));
  switch (
    if (intf) {
      Intf(Parse.interface(lexbuf));
    } else {
      Impl(Parse.implementation(lexbuf));
    }
  ) {
  | tree => Some(tree)
  | exception _ => None
  };
};

/* [file] is relative to [root]; [build] is the build context directory,
   where dune keeps a file's preprocessed forms next to its copy. */
let tree = (~root: string, ~build: string, file: string): option(tree) => {
  let has = Filename.check_suffix(file);
  let base = Filename.remove_extension(file);
  let intf = has(".mli") || has(".rei");
  let read = name => {
    let path = Filename.concat(build, name);
    if (intf) {
      Option.map(
        t => Intf(t),
        read_ast(path, ~magic=Config.ast_intf_magic_number),
      );
    } else {
      Option.map(
        t => Impl(t),
        read_ast(path, ~magic=Config.ast_impl_magic_number),
      );
    };
  };
  let preprocessed =
    if (has(".ml")) {
      [base ++ ".pp.ml"];
    } else if (has(".mli")) {
      [base ++ ".pp.mli"];
    } else if (has(".re")) {
      [file ++ ".pp.ml", file ++ ".ml"];
    } else if (has(".rei")) {
      [file ++ ".pp.mli", file ++ ".mli"];
    } else {
      [];
    };
  switch (List.find_map(read, preprocessed)) {
  | Some(tree) => Some(tree)
  | None when has(".ml") || has(".mli") =>
    Option.bind(
      read_file(Filename.concat(root, file)),
      parse_text(~file, ~intf),
    )
  | None => None
  };
};

/* ---------- from the text ---------- */

/* A scan for binders by indentation. A line opens an item of a module
   when it starts with [let], [and], [type], [external] or [val] at the
   indentation of that module's body; [module M = ...] opens a module,
   which lasts until a line indented no deeper than it. An item runs to
   the line before the next line at its indentation that opens something
   (an item, a module, [open], a comment, ...) or before the first line
   indented less. A module whose signature is written out before its body
   ([module M : sig ... end = struct ... end]) is found by the line where
   the body starts. Handles both syntaxes; knows nothing of strings or
   comments that span lines. */

let is_ident_char = c =>
  switch (c) {
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '_'
  | '\'' => true
  | _ => false
  };

/* The identifier at [i], and where it ends. */
let ident_at = (s: string, i: int): (string, int) => {
  let n = String.length(s);
  let rec stop = j =>
    if (j < n && is_ident_char(s.[j])) {
      stop(j + 1);
    } else {
      j;
    };
  let j = stop(i);
  (String.sub(s, i, j - i), j);
};

let rec skip_spaces = (s: string, i: int): int =>
  if (i < String.length(s) && (s.[i] == ' ' || s.[i] == '\t')) {
    skip_spaces(s, i + 1);
  } else {
    i;
  };

type line =
  | Blank
  | Item(int, kind, string) /* indentation, kind, name ("": a test without one) */
  | Module(int, string)
  /* [end = struct], [} = {]: a module's signature ends, its body starts */
  | Body(int)
  | Opener(int) /* starts something that is not an item: open, a comment, ... */
  | Other(int);

let openers = [
  "let",
  "and",
  "type",
  "module",
  "external",
  "val",
  "open",
  "include",
  "exception",
  "class",
];

/* [last] is the kind of the item an [and] continues. */
let classify = (~last: kind, s: string): line => {
  let n = String.length(s);
  let d = skip_spaces(s, 0);
  let starts = prefix =>
    d
    + String.length(prefix) <= n
    && String.sub(s, d, String.length(prefix)) == prefix;
  let ends = suffix => {
    let t = String.trim(s);
    String.ends_with(~suffix, t);
  };
  if (d >= n || s.[d] == '\r') {
    Blank;
  } else if (starts("}")
             && ends("= {")
             || starts("end")
             && ends("= struct")) {
    Body(d);
  } else if (starts("[@@") && !starts("[@@@")) {
    Other
      (d); /* an attribute of the item above */
  } else if (List.exists(starts, ["(*", "/*", "//", "[@", "[%%", "#"])) {
    Opener(d);
  } else {
    let (word, after) = ident_at(s, d);
    let after_word = skip_spaces(s, after);
    /* the name of a value: [rec] aside, the next identifier, or an
       operator in parentheses */
    let value_name = i => {
      let (w, j) = ident_at(s, i);
      let (w, j) =
        if (w == "rec") {
          let i = skip_spaces(s, j);
          let (w, j) = ident_at(s, i);
          (w, w == "" ? i : j);
        } else {
          (w, w == "" ? i : j);
        };
      let operator =
        if (w == "" && j < n && s.[j] == '(') {
          switch (String.index_from_opt(s, j, ')')) {
          | Some(close) =>
            let op = String.trim(String.sub(s, j + 1, close - j - 1));
            let symbol = c => !is_ident_char(c) && c != ' ' && c != ',';
            if (op != "" && String.for_all(symbol, op)) {
              Some(op);
            } else {
              None;
            };
          | None => None
          };
        } else {
          None;
        };
      switch (operator) {
      | Some(op) => Item(d, Val, op)
      | None when w == "" || w == "_" => Opener(d)
      | None => Item(d, Val, w)
      };
    };
    /* the name of a type: OCaml writes its parameters first */
    let type_name = i => {
      let (w, j) = ident_at(s, i);
      let i =
        if (w == "nonrec") {
          skip_spaces(s, j);
        } else {
          i;
        };
      let rec skip_params = i =>
        if (i >= n) {
          i;
        } else if (s.[i] == '(') {
          switch (String.index_from_opt(s, i, ')')) {
          | Some(close) => skip_params(skip_spaces(s, close + 1))
          | None => n
          };
        } else if (s.[i] == '\''
                   || s.[i] == '+'
                   || s.[i] == '-'
                   || s.[i] == '!') {
          let rec past = j =>
            if (j < n
                && (
                  is_ident_char(s.[j])
                  || s.[j] == '+'
                  || s.[j] == '-'
                  || s.[j] == '!'
                )) {
              past(j + 1);
            } else {
              j;
            };
          skip_params(skip_spaces(s, past(i)));
        } else {
          i;
        };
      let (w, _) = ident_at(s, skip_params(i));
      if (w == "") {
        Opener(d);
      } else {
        Item(d, Type, w);
      };
    };
    switch (word) {
    | "let" when after < n && s.[after] == '%' =>
      /* let%test "name" = ... */
      let (ext, j) = ident_at(s, after + 1);
      let j = skip_spaces(s, j);
      if ((ext == "test" || ext == "test_unit") && j < n && s.[j] == '"') {
        switch (String.index_from_opt(s, j + 1, '"')) {
        | Some(close) => Item(d, Test, String.sub(s, j + 1, close - j - 1))
        | None => Opener(d)
        };
      } else if (ext == "test" || ext == "test_unit") {
        Item(d, Test, "");
      } else {
        Opener(d);
      };
    | "let"
    | "external"
    | "val" => value_name(after_word)
    | "type" => type_name(after_word)
    | "and" =>
      switch (last) {
      | Type => type_name(after_word)
      | Val
      | Test => value_name(after_word)
      }
    | "module" =>
      let (w, j) = ident_at(s, after_word);
      let (w, _) =
        if (w == "rec") {
          ident_at(s, skip_spaces(s, j));
        } else {
          (w, j);
        };
      if (w == "" || w == "type") {
        Opener(d);
      } else {
        Module(d, w);
      };
    | w when List.mem(w, openers) => Opener(d)
    | _ => Other(d)
    };
  };
};

let scan = (~prefix: list(string), text: string): t => {
  let spans = Hashtbl.create(64);
  let lines = Array.of_list(String.split_on_char('\n', text));
  let n = Array.length(lines);
  /* classify, carrying the kind an [and] continues */
  let last = ref(Val);
  let classes =
    Array.map(
      s => {
        let c = classify(~last=last^, s);
        switch (c) {
        | Item(_, (Type | Val) as k, _) => last := k
        | _ => ()
        };
        c;
      },
      lines,
    );
  let indent =
    fun
    | Blank => None
    | Item(d, _, _)
    | Module(d, _)
    | Body(d)
    | Opener(d)
    | Other(d) => Some(d);
  /* the last line of the item that starts on line [i] (0-based) */
  let last_line = (i, d) => {
    let rec go = (j, last) =>
      if (j >= n) {
        last;
      } else {
        switch (classes[j]) {
        | Blank => go(j + 1, last)
        | c =>
          let dj = Option.get(indent(c));
          let opens =
            switch (c) {
            | Other(_) => false
            | _ => true
            };
          if (dj < d || dj == d && opens) {
            last;
          } else {
            go(j + 1, j);
          };
        };
      };
    go(i + 1, i);
  };
  /* open modules, innermost first: (indentation, name, body indentation) */
  let stack = ref([]);
  Array.iteri(
    (i, c) =>
      switch (indent(c)) {
      | None => ()
      | Some(d) =>
        stack :=
          List.filter(
            ((md, _, _)) =>
              switch (c) {
              /* the module it is the body of stays open, and what its
                 signature declared is found again, defined, below */
              | Body(_) => md <= d
              | _ => md < d
              },
            stack^,
          );
        switch (c, stack^) {
        | (Body(_), [(md, name, _), ...rest]) when md == d =>
          stack := [(md, name, None), ...rest]
        | _ => ()
        };
        let body =
          switch (stack^) {
          | [] => 0
          | [(md, _, None), ..._] when md == d => (-1) /* the [Body] line itself */
          | [(md, name, None), ...rest] =>
            stack := [(md, name, Some(d)), ...rest];
            d;
          | [(_, _, Some(body)), ..._] => body
          };
        let mpath = prefix @ List.rev_map(((_, name, _)) => name, stack^);
        if (d == body) {
          switch (c) {
          | Item(_, Test, "") =>
            add_unnamed_lines(spans, mpath, i + 1, last_line(i, d) + 1)
          | Item(_, kind, name) =>
            add(spans, kind, mpath, name, i + 1, last_line(i, d) + 1)
          | Module(_, name) => stack := [(d, name, None), ...stack^]
          | _ => ()
          };
        };
      },
    classes,
  );
  {
    exact: false,
    spans,
  };
};

/* ---------- the entry point ---------- */

/* [prefix] is the name of the file's module as the graph shows it
   (["Geo"]): entry paths start with it. [None] if the file cannot be
   read at all. */
let source =
    (~root: string, ~build: string, ~prefix: list(string), file: string)
    : option(t) =>
  switch (tree(~root, ~build, file)) {
  | Some(tree) =>
    let spans = Hashtbl.create(64);
    switch (tree) {
    | Impl(s) => structure(spans, prefix, s)
    | Intf(s) => signature(spans, prefix, s)
    };
    Some({
      exact: true,
      spans,
    });
  | None =>
    Option.map(scan(~prefix), read_file(Filename.concat(root, file)))
  };
