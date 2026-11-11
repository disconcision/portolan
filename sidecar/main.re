/* The Portolan sidecar.

   One process, started for one project directory. It
   - watches the project's source files; on a change it builds the
     annotations into a build directory of its own, runs the extractor and
     the tests, and emits a new graph snapshot when the graph changed;
   - runs a coding agent (Claude Code in its headless streaming mode, or a
     scripted stand-in) and relays its stream;
   - serves the page and a server-sent event stream carrying both;
   - appends every event to the project's trace file, which is loaded
     again at the next start and which `replay` plays back.

   Files on disk are the truth: the model is re-derived from them after
   every change, whoever made it.

   The modules, each with its interface:
     Files     reading and writing files, folders git ignores
     Bus       events, the trace file, waiting readers
     Delta     the change from one JSON object to another
     Graphs    graph events: whole graphs and changes between them
     Run       a shell command with a timeout
     Project   the watched project: paths, state folder, opam switch
     Watcher   file changes -> build -> graph -> tests
     Permissions  what the user chose to always allow the agent
     Agent     Claude Code or a scripted stand-in, and what it last did
     Conversation  starting a new one, keeping the old trace
     Syntax    OCaml <-> Reason conversion of a slice of a file
     Http      the page, the event stream, the routes
   This file reads the arguments and starts them.

   Dependencies: unix, threads, str, yojson. The graph is treated as
   opaque JSON here; only the extractor and the canvas know its shape. */

let (/) = Filename.concat;

let usage = "usage:
  portolan serve <project-dir> [flags]   watch a dune project and serve its page
  portolan replay <trace.jsonl> [--port N] [--speed X]   play back a recorded trace
  portolan compact <trace.jsonl> <out.jsonl>   rewrite an older trace, which holds every graph
                                whole, with graph changes: much smaller, plays the same
  portolan help

flags for serve:
  --port N                      port to serve on (default 8791)
  --agent claude|script:FILE    Claude Code (the default), or a scripted stand-in
  --model NAME                  the model Claude Code uses
  --nudge FILE                  text added to the agent's instructions
  --allow PATTERN               let the agent use one more tool without asking: a Claude Code
                                allowed-tool pattern such as 'Bash(make:*)'. Repeatable.
  --project-switch NAME_OR_DIR  the opam switch the project's builds, tests and the agent's
                                shell run in. Default: the project's own _opam folder if it
                                has one, otherwise the switch Portolan itself runs in.
  --extract FILE                the extractor executable (default: the one built in --home)
  --home DIR                    the Portolan folder holding web/ and canvas/dist/

The project's compiler must be OCaml 5.2.x for now: the extractor reads compiled annotations.
Without asking, the agent may edit files, run any dune command, and run ls, cat, head, tail,
wc, grep, rg, find, mkdir, git status, git diff, git log, opam list and ocamlfind list.
The trace of a project is .portolan/trace.jsonl in its folder; it carries on across restarts.";

type options = {
  mutable port: int,
  mutable home: option(string),
  mutable agent: string,
  mutable model: option(string),
  mutable nudge: option(string),
  mutable allow: list(string),
  mutable project_switch: option(string),
  mutable extractor: option(string),
  mutable speed: float,
};

let absolute = p => Filename.is_relative(p) ? Sys.getcwd() / p : p;

let fail = message => {
  prerr_endline("portolan: " ++ message);
  exit(2);
};

/* Reads the flags into [o]; returns the arguments that are not flags. */
let rec flags = o =>
  fun
  | ["--port", n, ...rest] => {
      switch (int_of_string_opt(n)) {
      | Some(n) => o.port = n
      | None => fail("--port takes a number")
      };
      flags(o, rest);
    }
  | ["--home", dir, ...rest] => {
      o.home = Some(absolute(dir));
      flags(o, rest);
    }
  | ["--agent", kind, ...rest] => {
      o.agent = kind;
      flags(o, rest);
    }
  | ["--model", name, ...rest] => {
      o.model = Some(name);
      flags(o, rest);
    }
  | ["--nudge", file, ...rest] => {
      o.nudge = Some(absolute(file));
      flags(o, rest);
    }
  | ["--allow", pattern, ...rest] => {
      o.allow = o.allow @ [pattern];
      flags(o, rest);
    }
  | ["--project-switch", switch_, ...rest] => {
      o.project_switch = Some(switch_);
      flags(o, rest);
    }
  | ["--extract", file, ...rest] => {
      o.extractor = Some(absolute(file));
      flags(o, rest);
    }
  | ["--speed", x, ...rest] => {
      switch (float_of_string_opt(x)) {
      | Some(x) when x > 0. => o.speed = x
      | _ => fail("--speed takes a positive number")
      };
      flags(o, rest);
    }
  | [arg, ...rest] => [arg, ...flags(o, rest)]
  | [] => [];

/* The Portolan folder: the nearest one above the executable holding the page. */
let find_home = () => {
  let rec up = (d, n) =>
    if (Sys.file_exists(d / "web" / "index.html")) {
      d;
    } else if (n == 0 || d == "/") {
      Sys.getcwd();
    } else {
      up(Filename.dirname(d), n - 1);
    };
  up(Filename.dirname(Unix.realpath(Sys.executable_name)), 6);
};

let serve = (o, ~home, dir) => {
  let kind =
    switch (Agent.kind_of_string(o.agent)) {
    | Some(Agent.Script(file)) => Agent.Script(absolute(file))
    | Some(kind) => kind
    | None => fail("--agent takes claude or script:FILE")
    };
  let project =
    switch (Project.at(~opam_switch=?o.project_switch, dir)) {
    | Ok(project) => project
    | Error(message) => fail(message)
    };
  let earlier = Bus.record(Project.trace(project));
  Bus.emit([
    ("type", `String("session")),
    ("restart", `Bool(earlier > 0)),
  ]);
  Bus.emit(
    [
      ("type", `String("hello")),
      ("project", `String(Project.name(project))),
      ("agent", `String(o.agent)),
      ("mode", `String("live")),
    ]
    @ (
      switch (Project.opam_switch(project)) {
      | Some(name) => [("switch", `String(name))]
      | None => []
      }
    ),
  );
  if (earlier > 0) {
    /* Whatever the agent was doing when the last session ended, it is not now. */
    Bus.emit([
      ("type", `String("agent_state")),
      ("state", `String("idle")),
    ]);
  };
  /* A project that had no `dune-project` when first served is a new one,
     and stays one across restarts: its agent gets the starting hints. */
  let started_empty = Project.state(project, "new");
  if (earlier == 0
      && !
           Sys.file_exists(
             Filename.concat(Project.root(project), "dune-project"),
           )) {
    Files.write(started_empty, "");
  };
  let agent =
    Agent.make(
      ~kind,
      ~model=?o.model,
      ~instructions=
        [home / "sidecar" / "agent-prompt.md"]
        @ (
          Sys.file_exists(started_empty)
            ? [home / "sidecar" / "agent-prompt-new.md"] : []
        )
        @ Option.to_list(o.nudge),
      ~allow=o.allow,
      project,
    );
  Watcher.start(
    ~extractor=
      Option.value(
        o.extractor,
        ~default=home / "_build" / "default" / "extract" / "main.exe",
      ),
    ~cause=() => Agent.take_cause(agent),
    project,
  );
  Http.serve(~port=o.port, ~home, Http.Live(project, agent));
};

let replay = (o, ~home, file) => {
  let lines =
    try(Files.lines(Files.read(absolute(file)))) {
    | Sys_error(message) => fail(message)
    };
  Http.serve(
    ~port=o.port,
    ~home,
    Http.Replay({
      lines,
      speed: o.speed,
    }),
  );
};

let compact = (file, out) => {
  let lines =
    try(Files.lines(Files.read(absolute(file)))) {
    | Sys_error(message) => fail(message)
    };
  Files.write(
    absolute(out),
    String.concat("\n", Graphs.compact(lines)) ++ "\n",
  );
};

let () = {
  Sys.set_signal(Sys.sigpipe, Sys.Signal_ignore);
  let o = {
    port: 8791,
    home: None,
    agent: "claude",
    model: None,
    nudge: None,
    allow: [],
    project_switch: None,
    extractor: None,
    speed: 1.0,
  };
  let positional = flags(o, List.tl(Array.to_list(Sys.argv)));
  let home =
    switch (o.home) {
    | Some(home) => home
    | None => find_home()
    };
  switch (positional) {
  | ["serve", dir] => serve(o, ~home, dir)
  | ["replay", file] => replay(o, ~home, file)
  | ["compact", file, out] => compact(file, out)
  | ["help" | "--help" | "-h", ..._] => print_endline(usage)
  | _ =>
    prerr_endline(usage);
    exit(2);
  };
};
