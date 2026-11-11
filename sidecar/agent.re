let (/) = Filename.concat;

type kind =
  | Claude
  | Script(string);

let kind_of_string = s => {
  let prefix = "script:";
  let n = String.length(prefix);
  if (s == "claude") {
    Some(Claude);
  } else if (String.length(s) > n && String.starts_with(~prefix, s)) {
    Some(Script(String.sub(s, n, String.length(s) - n)));
  } else {
    None;
  };
};

type about =
  Event.about = {
    name: string,
    where: option((string, int, int)),
  };

type answer =
  Event.answer =
    | Once
    | Always(list(string))
    | Deny
    | Answers(list((string, string)));

type t = {
  project: Project.t,
  kind,
  model: option(string),
  instructions: list(string),
  allow: list(string),
  /* Guards every mutable field but [cause]. */
  m: Mutex.t,
  mutable input: option(out_channel),
  mutable pid: option(int),
  /* Claude Code's conversation in this project, to carry on with. */
  mutable session: option(string),
  /* The state last put on the bus. */
  mutable state: Event.state,
  /* Counts finished turns, so a stop can tell whether its turn ended. */
  mutable turns: int,
  /* Requests waiting for the user's answer: id -> the tool's input. */
  pending: Hashtbl.t(string, Yojson.Safe.t),
  /* For the scripted stand-in: answers given, a stop asked for, which
     run is the current one, and the signal that one of these changed. */
  answered: Hashtbl.t(string, answer),
  mutable halt: bool,
  mutable run: int,
  wake: Condition.t,
  /* What the agent last did to the files; guarded by its own lock. */
  cause_m: Mutex.t,
  mutable cause: option(Event.cause),
};

let default_allow =
  ["Bash(dune:*)"]
  @ List.map(
      command => "Bash(" ++ command ++ ":*)",
      [
        "ls",
        "cat",
        "head",
        "tail",
        "wc",
        "grep",
        "rg",
        "find",
        "mkdir",
        "git status",
        "git diff",
        "git log",
        "opam list",
        "ocamlfind list",
      ],
    );

/* Holds the id of Claude Code's session in this project, once it has one. */
let session_file = a => Project.state(a.project, "claude-session");

let make = (~kind, ~model=?, ~instructions, ~allow, project) => {
  let a = {
    project,
    kind,
    model,
    instructions,
    allow,
    m: Mutex.create(),
    input: None,
    pid: None,
    session: None,
    state: Idle,
    turns: 0,
    pending: Hashtbl.create(4),
    answered: Hashtbl.create(4),
    halt: false,
    run: 0,
    wake: Condition.create(),
    cause_m: Mutex.create(),
    cause: None,
  };
  a.session = (
    switch (Files.lines(Files.read(session_file(a)))) {
    | [id, ..._] => Some(String.trim(id))
    | []
    | exception (Sys_error(_)) => None
    }
  );
  a;
};

let locked = (a, f) => {
  Mutex.lock(a.m);
  Fun.protect(~finally=() => Mutex.unlock(a.m), f);
};

/* Reports the agent's state, if it changed. Call with [a.m] held. */
let state = (a, s) =>
  if (a.state != s) {
    a.state = s;
    Bus.emit(Agent_state(s));
  };

/* ---------- what the agent did ---------- */

let set_cause = (a, c) => {
  Mutex.lock(a.cause_m);
  a.cause = Some(c);
  Mutex.unlock(a.cause_m);
};

let take_cause = a => {
  Mutex.lock(a.cause_m);
  let c = a.cause;
  a.cause = None;
  Mutex.unlock(a.cause_m);
  Option.value(c, ~default=Event.By_hand);
};

/* Note which file a tool call is about to change, to label the next graph. */
let track = (a, ev) =>
  Option.iter(
    set_cause(a),
    Protocol.cause(~relative=Project.relative(a.project), ev),
  );

/* ---------- asking the user ---------- */

/* Puts a request on the bus and remembers it until it is answered.
   Call with [a.m] held. */
let ask = (a, ~id, ~tool, ~input, ~suggestions, ~tool_use) => {
  Hashtbl.replace(a.pending, id, input);
  Bus.emit(
    Ask({
      id,
      tool,
      input,
      /* A question is answered, not allowed. */
      patterns:
        tool == "AskUserQuestion"
          ? [] : Permissions.suggest(~tool, ~input, ~suggestions),
      tool_use,
    }),
  );
  state(a, Waiting);
};

/* Requests nobody will answer any more. Call with [a.m] held. */
let drop_pending = a => {
  Hashtbl.iter(
    (id, _) =>
      Bus.emit(
        Answer({
          id,
          answer: None,
        }),
      ),
    a.pending,
  );
  Hashtbl.reset(a.pending);
};

/* ---------- Claude Code's stream ---------- */

/* One line to Claude Code. Call with [a.m] held. */
let send = (a, message) =>
  switch (a.input) {
  | Some(oc) =>
    try(
      {
        output_string(oc, Protocol.to_string(message));
        output_char(oc, '\n');
        flush(oc);
      }
    ) {
    | Sys_error(_) => ()
    }
  | None => ()
  };

/* One event of the agent's stream onto the bus, noting what it means for
   the agent's state. */
let event = (a, ev: Yojson.Safe.t) => {
  open Yojson.Safe.Util;
  track(a, ev);
  Bus.emit(Agent(ev));
  switch (ev |> member("type"), ev |> member("subtype")) {
  | (`String("system"), `String("init")) =>
    locked(
      a,
      () => {
        /* Claude Code announces its session at the start of each turn. */
        if (a.kind == Claude) {
          switch (ev |> member("session_id")) {
          | `String(id) when a.session != Some(id) =>
            a.session = Some(id);
            Files.write(session_file(a), id ++ "\n");
          | _ => ()
          };
        };
        state(a, Running);
      },
    )
  | (`String("result"), _) =>
    locked(
      a,
      () => {
        a.turns = a.turns + 1;
        drop_pending(a);
        state(a, Idle);
      },
    )
  | _ => ()
  };
};

/* One line of Claude Code's output. */
let relay = (a, line) =>
  switch (Protocol.read(line)) {
  | Asked({id, tool, input, suggestions, tool_use}) =>
    locked(a, () => ask(a, ~id, ~tool, ~input, ~suggestions, ~tool_use))
  | Asked_other(id) => locked(a, () => send(a, Protocol.refusal(~id)))
  | Withdrew(id) =>
    locked(a, () =>
      if (Hashtbl.mem(a.pending, id)) {
        Hashtbl.remove(a.pending, id);
        Bus.emit(
          Answer({
            id,
            answer: None,
          }),
        );
      }
    )
  | Began(ev, _)
  | Ended(ev)
  | Told(ev) => event(a, ev)
  | Raw(text) => Bus.emit(Agent_raw(Event.clip(text)))
  | Nothing => ()
  };

/* ---------- Claude Code ---------- */

let instructions = a =>
  a.instructions
  |> List.filter(Sys.file_exists)
  |> List.map(Files.read)
  |> String.concat("\n");

/* Starts the process and a thread relaying its output. Call with [a.m] held. */
let start_claude = a => {
  let (in_r, in_w) = Unix.pipe(~cloexec=true, ());
  let (out_r, out_w) = Unix.pipe(~cloexec=true, ());
  let err =
    Unix.openfile(
      Project.state(a.project, "agent.err"),
      [Unix.O_WRONLY, Unix.O_CREAT, Unix.O_APPEND],
      0o644,
    );
  let q = Filename.quote;
  let resumed = a.session;
  let command =
    String.concat(
      " ",
      [
        "cd",
        q(Project.root(a.project)),
        "&&",
        "exec",
        "claude",
        "-p",
        "--input-format",
        "stream-json",
        "--output-format",
        "stream-json",
        "--verbose",
        "--include-partial-messages",
        "--permission-mode",
        "acceptEdits",
        /* Ask on this stream before using a tool that is not allowed. */
        "--permission-prompt-tool",
        "stdio",
      ]
      @ (
        switch (resumed) {
        | Some(id) => ["--resume", q(id)]
        | None => []
        }
      )
      @ (
        switch (a.model) {
        | Some(model) => ["--model", q(model)]
        | None => []
        }
      )
      @ (
        switch (instructions(a)) {
        | "" => []
        | text => ["--append-system-prompt", q(text)]
        }
      )
      @ ["--allowedTools"]
      @ List.map(
          q,
          default_allow @ a.allow @ Permissions.remembered(a.project),
        ),
    );
  let argv = [|"/bin/sh", "-c", command|];
  let pid =
    switch (Project.env(a.project)) {
    | Some(env) =>
      Unix.create_process_env("/bin/sh", argv, env, in_r, out_w, err)
    | None => Unix.create_process("/bin/sh", argv, in_r, out_w, err)
    };
  Unix.close(in_r);
  Unix.close(out_w);
  Unix.close(err);
  let oc = Unix.out_channel_of_descr(in_w);
  a.input = Some(oc);
  a.pid = Some(pid);
  let ic = Unix.in_channel_of_descr(out_r);
  /* Not after the agent has been told to forget this process. */
  let current = () => locked(a, () => a.pid == Some(pid));
  let reader = () => {
    let spoke = ref(false);
    try(
      while (true) {
        let line = input_line(ic);
        if (current()) {
          relay(a, line);
        };
        spoke := true;
      }
    ) {
    | End_of_file
    | Sys_error(_) => ()
    };
    close_in_noerr(ic);
    try(ignore(Unix.waitpid([], pid))) {
    | Unix.Unix_error(_) => ()
    };
    close_out_noerr(oc);
    locked(a, () =>
      if (a.pid == Some(pid)) {
        a.pid = None;
        a.input = None;
        drop_pending(a);
        if (resumed != None && ! spoke^) {
          /* Nothing to carry on with after all: the next prompt starts afresh. */
          a.session = None;
          try(Sys.remove(session_file(a))) {
          | Sys_error(_) => ()
          };
          Bus.emit(
            Error({
              where: "agent",
              message: "Claude Code could not continue the earlier conversation (see .portolan/agent.err). Send the prompt again to start a new one.",
            }),
          );
        };
        state(a, Exited);
      }
    );
  };
  ignore(Thread.create(reader, ()));
};

/* ---------- the scripted stand-in ---------- */

exception Stopped;

/* Plays steps from a JSON file, doing the file operations itself and
   emitting events shaped like Claude Code's. */
let run_script = (a, file) => {
  open Yojson.Safe.Util;
  let run =
    locked(
      a,
      () => {
        a.run = a.run + 1;
        a.halt = false;
        state(a, Running);
        a.run;
      },
    );
  /* Ends the run when it was stopped; false once it has been forgotten. */
  let live = () =>
    locked(a, () =>
      if (a.run != run) {
        false;
      } else if (a.halt) {
        raise(Stopped);
      } else {
        true;
      }
    );
  let message = (kind, blocks) =>
    if (live()) {
      event(
        a,
        `Assoc([
          ("type", `String(kind)),
          (
            "message",
            `Assoc([("role", `String(kind)), ("content", `List(blocks))]),
          ),
        ]),
      );
    };
  let n = ref(0);
  let tool = (name, input) => {
    incr(n);
    let id = Printf.sprintf("script_%d_%d", run, n^);
    message(
      "assistant",
      [
        `Assoc([
          ("type", `String("tool_use")),
          ("id", `String(id)),
          ("name", `String(name)),
          ("input", input),
        ]),
      ],
    );
    id;
  };
  let result = (~error=false, id, text) =>
    message(
      "user",
      [
        `Assoc([
          ("type", `String("tool_result")),
          ("tool_use_id", `String(id)),
          ("content", `String(text)),
          ("is_error", `Bool(error)),
        ]),
      ],
    );
  /* Asks as Claude Code would, and waits for the user. */
  let ask_user = (name, input) => {
    let id = tool(name, input);
    locked(
      a,
      () => {
        ask(
          a,
          ~id,
          ~tool=name,
          ~input,
          ~suggestions=`Null,
          ~tool_use=Some(id),
        );
        while (a.run == run && !a.halt && !Hashtbl.mem(a.answered, id)) {
          Condition.wait(a.wake, a.m);
        };
      },
    );
    switch (locked(a, () => Hashtbl.find_opt(a.answered, id))) {
    | Some(Deny) => result(~error=true, id, "The user denied this.")
    | Some(Answers(answers)) =>
      result(
        id,
        String.concat(
          "; ",
          List.map(((q, answer)) => q ++ " = " ++ answer, answers),
        ),
      )
    | Some(Once | Always(_)) => result(id, "Allowed.")
    | None => ignore(live())
    };
  };
  let abs = f => Filename.is_relative(f) ? Project.root(a.project) / f : f;
  let step =
    fun
    | `Assoc([("say", `String(s))]) =>
      message(
        "assistant",
        [`Assoc([("type", `String("text")), ("text", `String(s))])],
      )
    | `Assoc([("wait", `Int(w))]) =>
      /* In short naps, so a stop is noticed. */
      for (_ in 1 to max(1, Int.div(w, 100))) {
        if (live()) {
          Thread.delay(0.1);
        };
      }
    | `Assoc([("write", w)]) => {
        let f = abs(w |> member("file") |> to_string)
        and content = w |> member("content") |> to_string;
        let id =
          tool(
            "Write",
            `Assoc([
              ("file_path", `String(f)),
              ("content", `String(content)),
            ]),
          );
        Files.mkdir_p(Filename.dirname(f));
        Files.write(f, content);
        result(id, "File written.");
      }
    | `Assoc([("edit", e)]) => {
        let f = abs(e |> member("file") |> to_string);
        let o = e |> member("old") |> to_string
        and nw = e |> member("new") |> to_string;
        let id =
          tool(
            "Edit",
            `Assoc([
              ("file_path", `String(f)),
              ("old_string", `String(o)),
              ("new_string", `String(nw)),
            ]),
          );
        let s = Files.read(f);
        switch (Str.search_forward(Str.regexp_string(o), s, 0)) {
        | i =>
          let after = i + String.length(o);
          Files.write(
            f,
            String.sub(s, 0, i)
            ++ nw
            ++ String.sub(s, after, String.length(s) - after),
          );
          result(id, "Edited.");
        | exception Not_found =>
          result(~error=true, id, "Error: old text not found.")
        };
      }
    /* {"ask": {"tool": "Bash", "input": {…}}}: a tool that needs the
       user's leave; with the tool AskUserQuestion, a question. */
    | `Assoc([("ask", r)]) =>
      ask_user(r |> member("tool") |> to_string, r |> member("input"))
    | _ => ();
  let finish = error =>
    if (locked(a, () => a.run == run)) {
      event(
        a,
        `Assoc([
          ("type", `String("result")),
          ("subtype", `String(error ? "error_during_execution" : "success")),
          ("is_error", `Bool(error)),
        ]),
      );
    };
  switch (
    Yojson.Safe.from_file(file)
    |> member("steps")
    |> to_list
    |> List.iter(s =>
         if (live()) {
           step(s);
         }
       )
  ) {
  | () => finish(false)
  | exception Stopped => finish(true)
  | exception e =>
    Bus.emit(
      Error({
        where: "script",
        message: Printexc.to_string(e),
      }),
    );
    finish(true);
  };
};

/* ---------- prompts ---------- */

/* What the agent is told about the definition a message is about. */
let about_text = about =>
  "\n\n(The user sent this with `"
  ++ about.name
  ++ "` selected on the canvas"
  ++ (
    switch (about.where) {
    | Some((file, first, last)) =>
      Printf.sprintf(": %s, lines %d to %d", file, first, last)
    | None => ""
    }
  )
  ++ ".)";

let prompt = (a, ~about=?, text) => {
  Bus.emit(
    Prompt({
      text,
      about,
    }),
  );
  switch (a.kind) {
  | Script(file) => ignore(Thread.create(() => run_script(a, file), ()))
  | Claude =>
    locked(
      a,
      () => {
        if (a.input == None) {
          start_claude(a);
        };
        send(
          a,
          Protocol.prompt(
            text ++ Option.fold(~none="", ~some=about_text, about),
          ),
        );
        state(a, Running);
      },
    )
  };
};

let answer = (a, ~id, answer) =>
  locked(a, () =>
    switch (Hashtbl.find_opt(a.pending, id)) {
    | None => ()
    | Some(input) =>
      Hashtbl.remove(a.pending, id);
      switch (answer) {
      | Always(patterns) => Permissions.remember(a.project, patterns)
      | Once
      | Deny
      | Answers(_) => ()
      };
      Bus.emit(
        Answer({
          id,
          answer: Some(answer),
        }),
      );
      switch (a.kind) {
      | Claude => send(a, Protocol.answer(~id, ~input, answer))
      | Script(_) =>
        Hashtbl.replace(a.answered, id, answer);
        Condition.broadcast(a.wake);
      };
      if (Hashtbl.length(a.pending) == 0) {
        state(a, Running);
      };
    }
  );

/* How long a stopped turn gets to end by itself. */
let grace = 5.;

let stop = a =>
  locked(a, () =>
    switch (a.kind, a.pid) {
    | (Script(_), _) =>
      a.halt = true;
      Condition.broadcast(a.wake);
    | (Claude, Some(pid)) when a.state != Idle =>
      /* Ends the turn and keeps the process and its conversation. */
      send(a, Protocol.interrupt(~id=Printf.sprintf("stop-%d", a.turns)));
      let turn = a.turns;
      let insist = () => {
        Thread.delay(grace);
        locked(a, () =>
          if (a.pid == Some(pid) && a.turns == turn && a.state != Idle) {
            try(Unix.kill(pid, Sys.sigterm)) {
            | Unix.Unix_error(_) => ()
            };
          }
        );
      };
      ignore(Thread.create(insist, ()));
    | _ => ()
    }
  );

let forget = a => {
  let pid =
    locked(
      a,
      () => {
        let pid = a.pid;
        Option.iter(close_out_noerr, a.input);
        a.pid = None;
        a.input = None;
        a.session = None;
        a.run = a.run + 1;
        Hashtbl.reset(a.pending);
        Hashtbl.reset(a.answered);
        Condition.broadcast(a.wake);
        a.state = Idle;
        try(Sys.remove(session_file(a))) {
        | Sys_error(_) => ()
        };
        pid;
      },
    );
  switch (pid) {
  | Some(pid) =>
    try(Unix.kill(pid, Sys.sigterm)) {
    | Unix.Unix_error(_) => ()
    }
  | None => ()
  };
};
