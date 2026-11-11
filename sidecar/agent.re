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

type t = {
  project: Project.t,
  kind,
  model: option(string),
  instructions: list(string),
  allow: list(string),
  /* Guards the three fields below. */
  m: Mutex.t,
  mutable input: option(out_channel),
  mutable pid: option(int),
  /* Whether Claude Code has a conversation here to continue. */
  mutable continues: bool,
  /* What the agent last did to the files; guarded by its own lock. */
  cause_m: Mutex.t,
  mutable cause: option(Yojson.Safe.t),
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
    continues: false,
    cause_m: Mutex.create(),
    cause: None,
  };
  a.continues = Sys.file_exists(session_file(a));
  a;
};

let state = s =>
  Bus.emit([("type", `String("agent_state")), ("state", `String(s))]);

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
  switch (c) {
  | Some(c) => c
  | None => `Assoc([("label", `String("edit")), ("by", `String("hand"))])
  };
};

/* Note which file a tool call is about to change, to label the next graph. */
let track = (a, ev: Yojson.Safe.t) =>
  Yojson.Safe.Util.(
    try(
      if (ev |> member("type") |> to_string == "assistant") {
        ev
        |> member("message")
        |> member("content")
        |> to_list
        |> List.iter(b =>
             if (b |> member("type") |> to_string == "tool_use") {
               let name = b |> member("name") |> to_string;
               let input = b |> member("input");
               let text = key =>
                 try(input |> member(key) |> to_string) {
                 | _ => ""
                 };
               switch (name) {
               | "Write"
               | "Edit"
               | "MultiEdit" =>
                 let file = text("file_path");
                 let file =
                   Option.value(
                     Project.relative(a.project, file),
                     ~default=file,
                   );
                 set_cause(
                   a,
                   `Assoc([
                     ("label", `String(name)),
                     ("tool", `String(name)),
                     ("file", `String(file)),
                     ("by", `String("agent")),
                   ]),
                 );
               | "Bash" =>
                 set_cause(
                   a,
                   `Assoc([
                     ("label", `String("Bash")),
                     ("tool", `String("Bash")),
                     (
                       "command",
                       `String(Bus.clip(~n=120, text("command"))),
                     ),
                     ("by", `String("agent")),
                   ]),
                 )
               | _ => ()
               };
             }
           );
      }
    ) {
    | _ => ()
    }
  );

/* ---------- the stream ---------- */

/* Claude Code announces its session first: from then on there is a
   conversation in this project to continue. */
let note_session = (a, ev: Yojson.Safe.t) =>
  Yojson.Safe.Util.(
    try(
      if (a.kind == Claude
          && ev
          |> member("type")
          |> to_string == "system"
          && ev
          |> member("subtype")
          |> to_string == "init") {
        Files.write(
          session_file(a),
          (ev |> member("session_id") |> to_string) ++ "\n",
        );
      }
    ) {
    | _ => ()
    }
  );

/* One line of the agent's output onto the bus. */
let relay = (a, line) =>
  switch (Yojson.Safe.from_string(line)) {
  | ev =>
    track(a, ev);
    note_session(a, ev);
    Bus.emit([("type", `String("agent")), ("ev", ev)]);
    try(
      if (Yojson.Safe.Util.(ev |> member("type") |> to_string) == "result") {
        state("idle");
      }
    ) {
    | _ => ()
    };
  | exception _ =>
    if (String.trim(line) != "") {
      Bus.emit([
        ("type", `String("agent_raw")),
        ("line", `String(Bus.clip(line))),
      ]);
    }
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
  let continued = a.continues;
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
      ]
      @ (continued ? ["--continue"] : [])
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
      @ List.map(q, default_allow @ a.allow),
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
  a.continues = true;
  a.input = Some(Unix.out_channel_of_descr(in_w));
  a.pid = Some(pid);
  state("running");
  let ic = Unix.in_channel_of_descr(out_r);
  let reader = () => {
    let spoke = ref(false);
    try(
      while (true) {
        relay(a, input_line(ic));
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
    Mutex.lock(a.m);
    if (a.pid == Some(pid)) {
      a.pid = None;
      a.input = None;
    };
    if (continued && ! spoke^) {
      /* Nothing to continue after all: the next prompt starts afresh. */
      a.continues = false;
      try(Sys.remove(session_file(a))) {
      | Sys_error(_) => ()
      };
      Bus.emit([
        ("type", `String("error")),
        ("where", `String("agent")),
        (
          "message",
          `String(
            "Claude Code could not continue the earlier conversation (see .portolan/agent.err). Send the prompt again to start a new one.",
          ),
        ),
      ]);
    };
    Mutex.unlock(a.m);
    state("exited");
  };
  ignore(Thread.create(reader, ()));
};

/* ---------- the scripted stand-in ---------- */

/* Plays steps from a JSON file, doing the file operations itself and
   emitting events shaped like Claude Code's. */
let run_script = (a, file) => {
  open Yojson.Safe.Util;
  let message = (kind, blocks) =>
    relay(
      a,
      Yojson.Safe.to_string(
        `Assoc([
          ("type", `String(kind)),
          (
            "message",
            `Assoc([("role", `String(kind)), ("content", `List(blocks))]),
          ),
        ]),
      ),
    );
  let n = ref(0);
  let tool = (name, input) => {
    incr(n);
    let id = Printf.sprintf("script_%d", n^);
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
  let result = (id, text) =>
    message(
      "user",
      [
        `Assoc([
          ("type", `String("tool_result")),
          ("tool_use_id", `String(id)),
          ("content", `String(text)),
        ]),
      ],
    );
  let abs = f => Filename.is_relative(f) ? Project.root(a.project) / f : f;
  state("running");
  try(
    Yojson.Safe.from_file(file)
    |> member("steps")
    |> to_list
    |> List.iter(step =>
         switch (step) {
         | `Assoc([("say", `String(s))]) =>
           message(
             "assistant",
             [`Assoc([("type", `String("text")), ("text", `String(s))])],
           )
         | `Assoc([("wait", `Int(w))]) =>
           Thread.delay(float_of_int(w) /. 1000.)
         | `Assoc([("write", w)]) =>
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
         | `Assoc([("edit", e)]) =>
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
           | exception Not_found => result(id, "Error: old text not found.")
           };
         | _ => ()
         }
       )
  ) {
  | e =>
    Bus.emit([
      ("type", `String("error")),
      ("where", `String("script")),
      ("message", `String(Printexc.to_string(e))),
    ])
  };
  relay(
    a,
    "{\"type\":\"result\",\"subtype\":\"success\",\"is_error\":false}",
  );
};

/* ---------- prompts ---------- */

let prompt = (a, text) => {
  Bus.emit([("type", `String("prompt")), ("text", `String(text))]);
  switch (a.kind) {
  | Script(file) => ignore(Thread.create(() => run_script(a, file), ()))
  | Claude =>
    Mutex.lock(a.m);
    if (a.input == None) {
      start_claude(a);
    };
    switch (a.input) {
    | Some(oc) =>
      let message =
        `Assoc([
          ("type", `String("user")),
          (
            "message",
            `Assoc([
              ("role", `String("user")),
              ("content", `String(text)),
            ]),
          ),
        ]);
      try(
        {
          output_string(oc, Yojson.Safe.to_string(message));
          output_char(oc, '\n');
          flush(oc);
          state("running");
        }
      ) {
      | Sys_error(_) => state("exited")
      };
    | None => ()
    };
    Mutex.unlock(a.m);
  };
};

let stop = a => {
  Mutex.lock(a.m);
  switch (a.pid) {
  | Some(pid) =>
    try(Unix.kill(pid, Sys.sigterm)) {
    | Unix.Unix_error(_) => ()
    }
  | None => ()
  };
  Mutex.unlock(a.m);
};
