let (/) = Filename.concat;

type t = {
  project: Project.t,
  extractor: string,
  cause: unit => Event.cause,
  /* The graph last sent, as compact JSON; "" before the first. */
  mutable last_graph: string,
  /* The failing tests of the last test run; None before the first. */
  mutable last_failing: option(list((string, int))),
};

/* ---------- which files count ---------- */

/* An OCaml or Reason file, as opposed to a dune file. */
let is_code = name =>
  List.exists(
    ext => Filename.check_suffix(name, ext),
    [".ml", ".mli", ".re", ".rei", ".mll", ".mly"],
  );

let is_source = name =>
  is_code(name)
  || List.mem(name, ["dune", "dune-project", "dune-workspace"]);

/* Build folders, hidden folders and the like are not the project's own. */
let is_skipped = name =>
  name == "" || name.[0] == '.' || name.[0] == '_' || name == "node_modules";

/* Every source file with its time and size: differs when anything changed. */
let signature = root => {
  let rec scan = (acc, dir) =>
    Array.fold_left(
      (acc, name) =>
        if (is_skipped(name)) {
          acc;
        } else {
          let p = dir / name;
          switch (Unix.lstat(p)) {
          | {Unix.st_kind: Unix.S_DIR, _} => scan(acc, p)
          | {Unix.st_kind: Unix.S_REG, st_mtime, st_size, _}
              when is_source(name) => [
              (p, st_mtime, st_size),
              ...acc,
            ]
          | _ => acc
          | exception (Unix.Unix_error(_)) => acc
          };
        },
      acc,
      try(Sys.readdir(dir)) {
      | Sys_error(_) => [||]
      },
    );
  List.sort(compare, scan([], root));
};

/* ---------- reading tool output ---------- */

/* Every `File "lib/scene.ml", line 21` in a compiler's or a test runner's
   output, as (file, line), in order, without repeats. */
let locations = out => {
  let acc = ref([]);
  let key = "File \"";
  let rec go = i =>
    switch (Str.search_forward(Str.regexp_string(key), out, i)) {
    | exception Not_found => ()
    | j =>
      let s = j + String.length(key);
      switch (String.index_from_opt(out, s, '"')) {
      | Some(e) =>
        let file = String.sub(out, s, e - s);
        let rest = String.sub(out, e, min(40, String.length(out) - e));
        try(
          /* "line 3" or, for an error over several lines, "lines 3-5". */
          Scanf.sscanf(rest, "\", line%_[s] %d", n =>
            if (!List.mem((file, n), acc^)) {
              acc := [(file, n), ...acc^];
            }
          )
        ) {
        | _ => ()
        };
      | None => ()
      };
      go(s + 1);
    };
  go(0);
  List.rev(acc^);
};

/* The locations that are in the project's own sources, relative to its
   root; those in build folders or outside the project are dropped. */
let in_project = (project, locs) =>
  List.filter_map(
    ((file, line)) => {
      let rel =
        Filename.is_relative(file)
          ? Some(file) : Project.relative(project, file);
      switch (rel) {
      | Some(rel)
          when
            !List.exists(is_skipped, String.split_on_char('/', rel))
            && Project.file(project, rel) != None =>
        Some((rel, line))
      | _ => None
      };
    },
    locs,
  );

/* ---------- the extractor ---------- */

let last_graph_file = ".portolan" / "last-graph.json";

/* Runs the extractor. [broken] is None after a good build; after a failed
   one it holds the error locations, and the extractor is also given the
   last graph to carry over what no longer compiles. */
let extract = (w, ~broken) => {
  let pairs = l =>
    String.concat(
      ",",
      List.map(((f, n)) => Printf.sprintf("%s:%d", f, n), l),
    );
  let flag = (name, value) => " " ++ name ++ " " ++ Filename.quote(value);
  let failing =
    switch (w.last_failing) {
    | Some(l) => flag("--failing", pairs(l))
    | None => ""
    };
  let carried =
    switch (broken) {
    | Some(locs) =>
      (
        Project.file(w.project, last_graph_file) != None
          ? flag("--previous", last_graph_file) : ""
      )
      ++ flag("--broken", pairs(locs))
    | None => ""
    };
  let errors = Project.state(w.project, "extract.err");
  let (code, out) =
    Run.cmd(
      ~timeout=60.,
      ~cwd=Project.root(w.project),
      Printf.sprintf(
        "%s . --build-dir %s%s%s 2>%s",
        Filename.quote(w.extractor),
        Project.build_dir,
        failing,
        carried,
        Filename.quote(errors),
      ),
    );
  let graph =
    try(code == 0 ? Some(Yojson.Safe.from_string(out)) : None) {
    | _ => None
    };
  switch (graph) {
  | Some(graph) => Ok(graph)
  | None =>
    let said =
      try(Files.read(errors)) {
      | Sys_error(_) => ""
      };
    Error(String.trim(said ++ "\n" ++ out));
  };
};

/* Whether the build folder holds any compiled annotation at all. */
let anything_compiled = project => {
  let rec any = dir =>
    Array.exists(
      name => {
        let p = dir / name;
        Filename.check_suffix(name, ".cmt")
        || Filename.check_suffix(name, ".cmti")
        || (
          try(Sys.is_directory(p)) {
          | Sys_error(_) => false
          }
        )
        && any(p);
      },
      try(Sys.readdir(dir)) {
      | Sys_error(_) => [||]
      },
    );
  any(Project.root(project) / Project.build_dir / "default");
};

/* Draws the graph and sends it, with its cause [why], if it is new. */
let emit_graph = (w, ~why, ~broken=?, ()) =>
  switch (extract(w, ~broken)) {
  | Ok(graph) =>
    let json = Yojson.Safe.to_string(graph);
    if (json != w.last_graph) {
      w.last_graph = json;
      Files.write(Project.root(w.project) / last_graph_file, json);
      Graphs.emit(~cause=why, ~partial=broken != None, graph);
    };
  /* A failed build with no graph to show for it: the page keeps the last
     picture. A good build with nothing compiled yet has nothing to say. */
  | Error(_) when broken != None || !anything_compiled(w.project) => ()
  | Error(output) => Bus.emit(Extract_failed(Event.clip(output)))
  };

/* ---------- one round ---------- */

let cycle = w =>
  if (Sys.file_exists(Project.root(w.project) / "dune-project")) {
    Project.ensure_build_dir(w.project);
    /* Asked once per change, sent or not, so a cause is never left over
       for a later change by someone else. */
    let why = w.cause();
    let started = Unix.gettimeofday();
    let (code, out) =
      Project.run(
        w.project,
        Printf.sprintf(
          "dune build --root . --build-dir %s @check 2>&1",
          Project.build_dir,
        ),
      );
    Bus.emit(
      Build({
        ok: code == 0,
        output: Event.clip(out),
        ms: int_of_float((Unix.gettimeofday() -. started) *. 1000.),
      }),
    );
    if (code == 0) {
      /* First with the test results we already had, so pips do not blink. */
      emit_graph(w, ~why, ());
      let (tcode, tout) =
        Project.run(
          ~timeout=90.,
          w.project,
          Printf.sprintf(
            "dune test --root . --build-dir %s 2>&1",
            Project.build_dir,
          ),
        );
      /* Dune names its own files too (the test rule in a `dune` file). */
      let failing = List.filter(((f, _)) => is_code(f), locations(tout));
      w.last_failing = Some(failing);
      Bus.emit(
        Tests({
          ok: tcode == 0,
          output: Event.clip(tout),
          failing,
        }),
      );
      emit_graph(w, ~why=Tested, ());
    } else {
      emit_graph(
        w,
        ~why,
        ~broken=in_project(w.project, locations(out)),
        (),
      );
    };
  };

/* ---------- start ---------- */

/* Where the last session left off, read from the bus's backlog. */
let resume = w => {
  switch (Graphs.resume(Bus.all())) {
  | Some(graph) =>
    w.last_graph = Yojson.Safe.to_string(graph);
    Files.write(Project.root(w.project) / last_graph_file, w.last_graph);
  | None =>
    /* No graph on the page: a file left from another trace is not its past. */
    try(Sys.remove(Project.root(w.project) / last_graph_file)) {
    | Sys_error(_) => ()
    }
  };
  switch (Bus.last(Tests)) {
  | Some(Tests({failing, _})) => w.last_failing = Some(failing)
  | _ => ()
  };
};

/* The extractor reads annotations written by the project's compiler, and
   only understands those of its own compiler version. */
let check_compiler = w =>
  switch (Project.run(~timeout=20., w.project, "ocamlc -version 2>/dev/null")) {
  | (0, version)
      when !String.starts_with(~prefix="5.2.", String.trim(version)) =>
    Bus.emit(
      Error({
        where: "switch",
        message:
          "the project's compiler is OCaml "
          ++ String.trim(version)
          ++ "; Portolan can only draw projects built with OCaml 5.2.x for now",
      }),
    )
  | _ => ()
  };

let start = (~extractor, ~cause, project) => {
  let w = {
    project,
    extractor,
    cause,
    last_graph: "",
    last_failing: None,
  };
  resume(w);
  let run = () => {
    check_compiler(w);
    let last = ref([])
    and settled = ref([]);
    while (true) {
      try({
        let s = signature(Project.root(project));
        /* Act once the tree has been the same for two looks in a row. */
        if (s != last^ && s == settled^) {
          last := s;
          cycle(w);
        };
        settled := s;
      }) {
      | e =>
        Bus.emit(
          Error({
            where: "watcher",
            message: Printexc.to_string(e),
          }),
        )
      };
      Thread.delay(0.25);
    };
  };
  ignore(Thread.create(run, ()));
};
