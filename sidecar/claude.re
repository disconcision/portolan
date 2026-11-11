type process = {
  pid: int,
  input: out_channel,
};

type t = {
  project: Project.t,
  model: option(string),
  instructions: list(string),
  allow: list(string),
  turn: Turn.t,
  /* Guards the mutable fields. Held while calling [Turn], never the
     other way round. */
  m: Mutex.t,
  mutable running: option(process),
  /* Claude Code's conversation in this project, to carry on with. */
  mutable session: option(string),
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
let session_file = c => Project.state(c.project, "claude-session");

let make = (~model=?, ~instructions, ~allow, project, turn) => {
  let c = {
    project,
    model,
    instructions,
    allow,
    turn,
    m: Mutex.create(),
    running: None,
    session: None,
  };
  c.session = (
    switch (Files.lines(Files.read(session_file(c)))) {
    | [id, ..._] => Some(String.trim(id))
    | []
    | exception (Sys_error(_)) => None
    }
  );
  c;
};

let locked = (c, f) => {
  Mutex.lock(c.m);
  Fun.protect(~finally=() => Mutex.unlock(c.m), f);
};

let command = (~root, ~resume, ~model, ~instructions, ~allow) => {
  let q = Filename.quote;
  String.concat(
    " ",
    [
      "cd",
      q(root),
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
      switch (resume) {
      | Some(id) => ["--resume", q(id)]
      | None => []
      }
    )
    @ (
      switch (model) {
      | Some(model) => ["--model", q(model)]
      | None => []
      }
    )
    @ (
      switch (instructions) {
      | "" => []
      | text => ["--append-system-prompt", q(text)]
      }
    )
    @ ["--allowedTools"]
    @ List.map(q, allow),
  );
};

/* One line to Claude Code. Call with [c.m] held. */
let send = (c, message) =>
  switch (c.running) {
  | Some({input, _}) =>
    try(
      {
        output_string(input, Protocol.to_string(message));
        output_char(input, '\n');
        flush(input);
      }
    ) {
    | Sys_error(_) => ()
    }
  | None => ()
  };

/* Call with [c.m] held. */
let is_running = (c, pid) =>
  switch (c.running) {
  | Some(process) => process.pid == pid
  | None => false
  };

/* One line of Claude Code's output. */
let heard = (c, line) =>
  switch (Protocol.read(line)) {
  | Began(ev, session) =>
    Turn.happened(c.turn, Told(ev));
    locked(c, () =>
      switch (session) {
      | Some(id) when c.session != Some(id) =>
        c.session = Some(id);
        Files.write(session_file(c), id ++ "\n");
      | _ => ()
      }
    );
    Turn.happened(c.turn, Began);
  | Ended(ev) => Turn.happened(c.turn, Ended(ev))
  | Told(ev) => Turn.happened(c.turn, Told(ev))
  | Asked(request) => Turn.happened(c.turn, Asked(request))
  | Asked_other(id) => locked(c, () => send(c, Protocol.refusal(~id)))
  | Withdrew(id) => Turn.happened(c.turn, Withdrew(id))
  | Raw(text) => Bus.emit(Agent_raw(Event.clip(text)))
  | Nothing => ()
  };

/* Starts the process and a thread relaying its output. Call with [c.m] held. */
let start = c => {
  let (in_r, in_w) = Unix.pipe(~cloexec=true, ());
  let (out_r, out_w) = Unix.pipe(~cloexec=true, ());
  let err =
    Unix.openfile(
      Project.state(c.project, "agent.err"),
      [Unix.O_WRONLY, Unix.O_CREAT, Unix.O_APPEND],
      0o644,
    );
  let resumed = c.session;
  let argv = [|
    "/bin/sh",
    "-c",
    command(
      ~root=Project.root(c.project),
      ~resume=resumed,
      ~model=c.model,
      ~instructions=
        c.instructions
        |> List.filter(Sys.file_exists)
        |> List.map(Files.read)
        |> String.concat("\n"),
      ~allow=default_allow @ c.allow @ Permissions.remembered(c.project),
    ),
  |];
  let pid =
    switch (Project.env(c.project)) {
    | Some(env) =>
      Unix.create_process_env("/bin/sh", argv, env, in_r, out_w, err)
    | None => Unix.create_process("/bin/sh", argv, in_r, out_w, err)
    };
  Unix.close(in_r);
  Unix.close(out_w);
  Unix.close(err);
  let input = Unix.out_channel_of_descr(in_w);
  c.running =
    Some({
      pid,
      input,
    });
  let output = Unix.in_channel_of_descr(out_r);
  let reader = () => {
    let spoke = ref(false);
    try(
      while (true) {
        let line = input_line(output);
        /* Not after the agent has been told to forget this process. */
        if (locked(c, () => is_running(c, pid))) {
          heard(c, line);
        };
        spoke := true;
      }
    ) {
    | End_of_file
    | Sys_error(_) => ()
    };
    close_in_noerr(output);
    try(ignore(Unix.waitpid([], pid))) {
    | Unix.Unix_error(_) => ()
    };
    close_out_noerr(input);
    locked(c, () =>
      if (is_running(c, pid)) {
        c.running = None;
        if (resumed != None && ! spoke^) {
          /* Nothing to carry on with after all: the next prompt starts afresh. */
          c.session = None;
          try(Sys.remove(session_file(c))) {
          | Sys_error(_) => ()
          };
          Bus.emit(
            Error({
              where: "agent",
              message: "Claude Code could not continue the earlier conversation (see .portolan/agent.err). Send the prompt again to start a new one.",
            }),
          );
        };
        Turn.happened(c.turn, Exited);
      }
    );
  };
  ignore(Thread.create(reader, ()));
};

let prompt = (c, text) =>
  locked(
    c,
    () => {
      if (c.running == None) {
        start(c);
      };
      /* Before the prompt goes, so that the end of a quick turn is not
         taken for its start. */
      Turn.happened(c.turn, Began);
      send(c, Protocol.prompt(text));
    },
  );

let answer = (c, ~id, ~input, answer) =>
  locked(c, () => send(c, Protocol.answer(~id, ~input, answer)));

/* The turn under way, as the number of turns ended before it. */
let under_way = c => {
  let standing = Turn.standing(c.turn);
  Turn.state(standing) == Idle ? None : Some(Turn.finished(standing));
};

/* How long a stopped turn gets to end by itself. */
let grace = 5.;

let stop = c =>
  locked(c, () =>
    switch (c.running, under_way(c)) {
    | (Some({pid, _}), Some(turn)) =>
      /* Ends the turn and keeps the process and its conversation. */
      send(c, Protocol.interrupt(~id=Printf.sprintf("stop-%d", turn)));
      let insist = () => {
        Thread.delay(grace);
        locked(c, () =>
          if (is_running(c, pid) && under_way(c) == Some(turn)) {
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

let forget = c => {
  let was =
    locked(
      c,
      () => {
        let was = c.running;
        Option.iter(process => close_out_noerr(process.input), was);
        c.running = None;
        c.session = None;
        try(Sys.remove(session_file(c))) {
        | Sys_error(_) => ()
        };
        was;
      },
    );
  switch (was) {
  | Some({pid, _}) =>
    try(Unix.kill(pid, Sys.sigterm)) {
    | Unix.Unix_error(_) => ()
    }
  | None => ()
  };
};
