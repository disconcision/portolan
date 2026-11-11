/* The Portolan sidecar.

   One process, started for one project directory. It
   - watches the project's source files; on a change it builds the
     annotations into a build directory of its own, runs the extractor and
     the tests, and emits a new graph snapshot when the graph changed;
   - runs a coding agent (Claude Code in its headless streaming mode, or a
     scripted stand-in) and relays its stream;
   - serves the page and a server-sent event stream carrying both;
   - appends every event to a trace file, which `replay` plays back.

   Files on disk are the truth: the model is re-derived from them after
   every change, whoever made it. A build that fails emits no new graph,
   so the canvas keeps its last good picture.

   Dependencies: unix, threads, yojson. The graph is treated as opaque
   JSON here; only the extractor and the canvas know its shape. */

let now = () => Unix.gettimeofday();
let t0 = now();
let ms = () => int_of_float((now() -. t0) *. 1000.);
let (/) = Filename.concat;

let read_file = path => {
  let ic = open_in_bin(path);
  Fun.protect(
    ~finally=() => close_in_noerr(ic),
    () => really_input_string(ic, in_channel_length(ic)),
  );
};

let write_file = (path, s) => {
  let oc = open_out_bin(path);
  Fun.protect(
    ~finally=() => close_out_noerr(oc),
    () => output_string(oc, s),
  );
};

let rec mkdir_p = d =>
  if (d != "" && d != "/" && !Sys.file_exists(d)) {
    mkdir_p(Filename.dirname(d));
    try(Unix.mkdir(d, 0o755)) {
    | [@implicit_arity] Unix.Unix_error(Unix.EEXIST, _, _) => ()
    };
  };

let truncate = (~n=6000, s) =>
  if (String.length(s) <= n) {
    s;
  } else {
    String.sub(s, 0, n) ++ "\n…";
  };

/* ---------- the event bus ---------- */

module Bus = {
  let m = Mutex.create();
  let c = Condition.create();
  let events: Dynarray.t(string) = (Dynarray.create(): Dynarray.t(string));
  let trace: ref(option(out_channel)) = (
    ref(None): ref(option(out_channel))
  );

  let emit = (fields: list((string, Yojson.Safe.t))) => {
    let s = Yojson.Safe.to_string(`Assoc([("t", `Int(ms())), ...fields]));
    Mutex.lock(m);
    Dynarray.add_last(events, s);
    switch (trace^) {
    | Some(oc) =>
      output_string(oc, s);
      output_char(oc, '\n');
      flush(oc);
    | None => ()
    };
    Condition.broadcast(c);
    Mutex.unlock(m);
  };

  /* Blocks until there are events at or past [from]; returns them. */
  let wait_from = from => {
    Mutex.lock(m);
    while (Dynarray.length(events) <= from) {
      Condition.wait(c, m);
    };
    let n = Dynarray.length(events);
    let batch = List.init(n - from, i => Dynarray.get(events, from + i));
    Mutex.unlock(m);
    batch;
  };

  let all = () => {
    Mutex.lock(m);
    let l = Dynarray.to_list(events);
    Mutex.unlock(m);
    l;
  };
};

/* ---------- running commands ---------- */

/* Runs a shell command in [cwd], returns (exit code, combined output).
   Killed after [timeout] seconds, reported as code 124. */
let run_cmd = (~timeout=120., ~cwd, cmd) => {
  let (r, w) = Unix.pipe(~cloexec=true, ());
  let sh = Printf.sprintf("cd %s && %s", Filename.quote(cwd), cmd);
  let devnull = Unix.openfile("/dev/null", [Unix.O_RDONLY], 0);
  let pid =
    Unix.create_process("/bin/sh", [|"/bin/sh", "-c", sh|], devnull, w, w);
  Unix.close(devnull);
  Unix.close(w);
  let buf = Buffer.create(4096)
  and chunk = Bytes.create(4096);
  let deadline = now() +. timeout;
  let timed_out = ref(false);
  let rec loop = () => {
    let left = deadline -. now();
    if (left <= 0.) {
      timed_out := true;
      try(Unix.kill(pid, Sys.sigkill)) {
      | Unix.Unix_error(_) => ()
      };
    } else {
      switch (Unix.select([r], [], [], Float.min(left, 1.0))) {
      | ([], _, _) => loop()
      | _ =>
        let n =
          try(Unix.read(r, chunk, 0, 4096)) {
          | Unix.Unix_error(_) => 0
          };
        if (n > 0) {
          Buffer.add_subbytes(buf, chunk, 0, n);
          loop();
        };
      | exception ([@implicit_arity] Unix.Unix_error(Unix.EINTR, _, _)) =>
        loop()
      };
    };
  };

  loop();
  Unix.close(r);
  let code =
    switch (snd(Unix.waitpid([], pid))) {
    | Unix.WEXITED(n) => n
    | _ => 125
    | exception (Unix.Unix_error(_)) => 125
    };

  (
    if (timed_out^) {
      124;
    } else {
      code;
    },
    Buffer.contents(buf),
  );
};

/* ---------- configuration ---------- */

let project = ref(""); /* absolute path of the watched project */
let home = ref(""); /* the portolan repository: web/, built extractor, canvas bundle */
let port = ref(8791);
let build_dir = "_build_portolan";
let agent_kind = ref("claude"); /* "claude" | "script:<file>" */
let model = ref("");
let extract_exe = ref("");

/* ---------- watcher: files -> build -> extract -> tests ---------- */

let is_source = name =>
  List.exists(
    ext => Filename.check_suffix(name, ext),
    [".ml", ".mli", ".re", ".rei", ".mll", ".mly"],
  )
  || List.mem(name, ["dune", "dune-project", "dune-workspace"]);

let signature = () => {
  let rec scan = (acc, dir) =>
    Array.fold_left(
      (acc, name) =>
        if (name == ""
            || name.[0] == '.'
            || name.[0] == '_'
            || name == "node_modules") {
          acc;
        } else {
          let p = dir / name;
          switch (Unix.lstat(p).Unix.st_kind) {
          | Unix.S_DIR => scan(acc, p)
          | Unix.S_REG when is_source(name) =>
            let st = Unix.lstat(p);
            [(p, st.Unix.st_mtime, st.Unix.st_size), ...acc];
          | _ => acc
          | exception (Unix.Unix_error(_)) => acc
          };
        },
      acc,
      try(Sys.readdir(dir)) {
      | Sys_error(_) => [||]
      },
    );

  List.sort(compare, scan([], project^));
};

let last_graph = ref("");
let last_failing: ref(option(list((string, int)))) = (
  ref(None): ref(option(list((string, int))))
);

/* What the agent last did to the files, to label the next graph change. */
let cause_m = Mutex.create();
let pending_cause: ref(option(Yojson.Safe.t)) = (
  ref(None): ref(option(Yojson.Safe.t))
);

let take_cause = () => {
  Mutex.lock(cause_m);
  let c = pending_cause^;
  pending_cause := None;
  Mutex.unlock(cause_m);
  switch (c) {
  | Some(c) => c
  | None => `Assoc([("label", `String("edit")), ("by", `String("hand"))])
  };
};

let set_cause = c => {
  Mutex.lock(cause_m);
  pending_cause := Some(c);
  Mutex.unlock(cause_m);
};

let extract = failing => {
  let failing_arg =
    switch (failing) {
    | None => ""
    | Some(l) =>
      " --failing "
      ++ Filename.quote(
           String.concat(
             ",",
             List.map(((f, n)) => Printf.sprintf("%s:%d", f, n), l),
           ),
         )
    };

  run_cmd(
    ~timeout=60.,
    ~cwd=project^,
    Printf.sprintf(
      "%s . --build-dir %s%s 2>/dev/null",
      Filename.quote(extract_exe^),
      build_dir,
      failing_arg,
    ),
  );
};

let emit_graph = (~why, failing) =>
  switch (extract(failing)) {
  | (0, json) when json != "" =>
    if (json != last_graph^) {
      switch (Yojson.Safe.from_string(json)) {
      | g =>
        last_graph := json;
        let cause =
          switch (why) {
          | Some(w) => w
          | None => take_cause()
          };
        Bus.emit([
          ("type", `String("graph")),
          ("graph", g),
          ("cause", cause),
        ]);
      | exception _ =>
        Bus.emit([
          ("type", `String("extract")),
          ("ok", `Bool(false)),
          ("output", `String(truncate(json))),
        ])
      };
    }
  | (_, out) =>
    Bus.emit([
      ("type", `String("extract")),
      ("ok", `Bool(false)),
      ("output", `String(truncate(out))),
    ])
  };

/* "File "lib/scene.ml", line 21, characters 0-58: ... is false." */
let parse_failing = out => {
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
          Scanf.sscanf(rest, "\", line %d", n =>
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

let cycle = () =>
  if (Sys.file_exists(project^ / "dune-project")) {
    let t = now();
    let (code, out) =
      run_cmd(
        ~cwd=project^,
        Printf.sprintf(
          "dune build --root . --build-dir %s @check 2>&1",
          build_dir,
        ),
      );

    Bus.emit([
      ("type", `String("build")),
      ("ok", `Bool(code == 0)),
      ("output", `String(truncate(out))),
      ("ms", `Int(int_of_float((now() -. t) *. 1000.))),
    ]);
    if (code == 0) {
      /* First with the test results we already had, so pips do not blink. */
      emit_graph(~why=None, last_failing^);
      let (tcode, tout) =
        run_cmd(
          ~timeout=90.,
          ~cwd=project^,
          Printf.sprintf("dune test --root . --build-dir %s 2>&1", build_dir),
        );

      let failing = parse_failing(tout);
      last_failing := Some(failing);
      Bus.emit([
        ("type", `String("tests")),
        ("ok", `Bool(tcode == 0)),
        ("output", `String(truncate(tout))),
        (
          "failing",
          `List(
            List.map(
              ((f, n)) =>
                `Assoc([("file", `String(f)), ("line", `Int(n))]),
              failing,
            ),
          ),
        ),
      ]);
      emit_graph(
        ~why=Some(`Assoc([("label", `String("tests"))])),
        Some(failing),
      );
    };
  };

let watcher = () => {
  let last = ref([])
  and settled = ref([]);
  while (true) {
    try({
      let s = signature();
      /* Act once the tree has been the same for two looks in a row. */
      if (s != last^ && s == settled^) {
        last := s;
        cycle();
      };
      settled := s;
    }) {
    | e =>
      Bus.emit([
        ("type", `String("error")),
        ("where", `String("watcher")),
        ("message", `String(Printexc.to_string(e))),
      ])
    };
    Thread.delay(0.25);
  };
};

/* ---------- the agent ---------- */

let agent_m = Mutex.create();
let agent_in: ref(option(out_channel)) = (
  ref(None): ref(option(out_channel))
);
let agent_pid: ref(option(int)) = (ref(None): ref(option(int)));
let started_once = ref(false);

let agent_state = s =>
  Bus.emit([("type", `String("agent_state")), ("state", `String(s))]);

let rel = path => {
  let p = project^ ++ "/";
  let n = String.length(p);
  if (String.length(path) > n && String.sub(path, 0, n) == p) {
    String.sub(path, n, String.length(path) - n);
  } else {
    path;
  };
};

/* Note which file a tool call is about to change, to label the next graph. */
let track = (ev: Yojson.Safe.t) =>
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
               switch (name) {
               | "Write"
               | "Edit"
               | "MultiEdit" =>
                 let f =
                   try(input |> member("file_path") |> to_string) {
                   | _ => ""
                   };
                 set_cause(
                   `Assoc([
                     ("label", `String(name)),
                     ("tool", `String(name)),
                     ("file", `String(rel(f))),
                     ("by", `String("agent")),
                   ]),
                 );
               | "Bash" =>
                 let c =
                   try(input |> member("command") |> to_string) {
                   | _ => ""
                   };
                 set_cause(
                   `Assoc([
                     ("label", `String("Bash")),
                     ("tool", `String("Bash")),
                     ("command", `String(truncate(~n=120, c))),
                     ("by", `String("agent")),
                   ]),
                 );
               | _ => ()
               };
             }
           );
      }
    ) {
    | _ => ()
    }
  );

let relay_line = line =>
  switch (Yojson.Safe.from_string(line)) {
  | ev =>
    track(ev);
    Bus.emit([("type", `String("agent")), ("ev", ev)]);
    try(
      if (Yojson.Safe.Util.(ev |> member("type") |> to_string) == "result") {
        agent_state("idle");
      }
    ) {
    | _ => ()
    };
  | exception _ =>
    if (String.trim(line) != "") {
      Bus.emit([
        ("type", `String("agent_raw")),
        ("line", `String(truncate(line))),
      ]);
    }
  };

let prompt_extra = ref(""); /* a file whose text is added to the agent's instructions */

let agent_prompt = () => {
  let f = home^ / "sidecar" / "agent-prompt.md";
  let base =
    if (Sys.file_exists(f)) {
      read_file(f);
    } else {
      "";
    };
  if (prompt_extra^ != "" && Sys.file_exists(prompt_extra^)) {
    base ++ "\n" ++ read_file(prompt_extra^);
  } else {
    base;
  };
};

let start_claude = () => {
  let (in_r, in_w) = Unix.pipe(~cloexec=true, ());
  let (out_r, out_w) = Unix.pipe(~cloexec=true, ());
  mkdir_p(project^ / ".portolan");
  let err =
    Unix.openfile(
      project^ / ".portolan" / "agent.err",
      [Unix.O_WRONLY, Unix.O_CREAT, Unix.O_APPEND],
      0o644,
    );
  let q = Filename.quote;
  let allowed = [
    "Bash(dune build:*)",
    "Bash(dune test:*)",
    "Bash(dune runtest:*)",
    "Bash(dune exec:*)",
    "Bash(dune clean:*)",
    "Bash(dune fmt:*)",
    "Bash(ls:*)",
    "Bash(mkdir:*)",
    "Bash(cat:*)",
  ];

  let cmd =
    String.concat(
      " ",
      [
        "cd",
        q(project^),
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
      @ (
        if (started_once^) {
          ["--continue"];
        } else {
          [];
        }
      )
      @ (
        if (model^ != "") {
          ["--model", q(model^)];
        } else {
          [];
        }
      )
      @ (
        switch (agent_prompt()) {
        | "" => []
        | p => ["--append-system-prompt", q(p)]
        }
      )
      @ ["--allowedTools"]
      @ List.map(q, allowed),
    );

  let pid =
    Unix.create_process(
      "/bin/sh",
      [|"/bin/sh", "-c", cmd|],
      in_r,
      out_w,
      err,
    );
  Unix.close(in_r);
  Unix.close(out_w);
  Unix.close(err);
  started_once := true;
  agent_in := Some(Unix.out_channel_of_descr(in_w));
  agent_pid := Some(pid);
  agent_state("running");
  let ic = Unix.in_channel_of_descr(out_r);
  ignore(
    Thread.create(
      () => {
        try(
          while (true) {
            relay_line(input_line(ic));
          }
        ) {
        | End_of_file
        | Sys_error(_) => ()
        };
        close_in_noerr(ic);
        try(ignore(Unix.waitpid([], pid))) {
        | Unix.Unix_error(_) => ()
        };
        Mutex.lock(agent_m);
        if (agent_pid^ == Some(pid)) {
          agent_pid := None;
          agent_in := None;
        };
        Mutex.unlock(agent_m);
        agent_state("exited");
      },
      (),
    ),
  );
};

/* A scripted stand-in: plays steps from a JSON file, doing the file
   operations itself and emitting events shaped like Claude Code's. */
let run_script = file => {
  open Yojson.Safe.Util;
  let assistant = blocks =>
    relay_line(
      Yojson.Safe.to_string(
        `Assoc([
          ("type", `String("assistant")),
          (
            "message",
            `Assoc([
              ("role", `String("assistant")),
              ("content", `List(blocks)),
            ]),
          ),
        ]),
      ),
    );

  let n = ref(0);
  let tool = (name, input) => {
    incr(n);
    let id = Printf.sprintf("script_%d", n^);
    assistant([
      `Assoc([
        ("type", `String("tool_use")),
        ("id", `String(id)),
        ("name", `String(name)),
        ("input", input),
      ]),
    ]);
    id;
  };

  let result = (id, text) =>
    relay_line(
      Yojson.Safe.to_string(
        `Assoc([
          ("type", `String("user")),
          (
            "message",
            `Assoc([
              ("role", `String("user")),
              (
                "content",
                `List([
                  `Assoc([
                    ("type", `String("tool_result")),
                    ("tool_use_id", `String(id)),
                    ("content", `String(text)),
                  ]),
                ]),
              ),
            ]),
          ),
        ]),
      ),
    );

  let abs = f =>
    if (Filename.is_relative(f)) {
      project^ / f;
    } else {
      f;
    };
  agent_state("running");
  try(
    Yojson.Safe.from_file(file)
    |> member("steps")
    |> to_list
    |> List.iter(step =>
         switch (step) {
         | `Assoc([("say", `String(s))]) =>
           assistant([
             `Assoc([("type", `String("text")), ("text", `String(s))]),
           ])
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
           mkdir_p(Filename.dirname(f));
           write_file(f, content);
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
           let s = read_file(f);
           switch (Str.search_forward(Str.regexp_string(o), s, 0)) {
           | i =>
             write_file(
               f,
               String.sub(s, 0, i)
               ++ nw
               ++ String.sub(
                    s,
                    i + String.length(o),
                    String.length(s) - i - String.length(o),
                  ),
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
  relay_line(
    "{\"type\":\"result\",\"subtype\":\"success\",\"is_error\":false}",
  );
};

let send_prompt = text => {
  Bus.emit([("type", `String("prompt")), ("text", `String(text))]);
  let script = {
    let k = "script:";
    let n = String.length(k);
    if (String.length(agent_kind^) > n && String.sub(agent_kind^, 0, n) == k) {
      Some(String.sub(agent_kind^, n, String.length(agent_kind^) - n));
    } else {
      None;
    };
  };

  switch (script) {
  | Some(file) => ignore(Thread.create(run_script, file))
  | None =>
    Mutex.lock(agent_m);
    if (agent_in^ == None) {
      start_claude();
    };
    switch (agent_in^) {
    | Some(oc) =>
      let msg =
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
          output_string(oc, Yojson.Safe.to_string(msg));
          output_char(oc, '\n');
          flush(oc);
          agent_state("running");
        }
      ) {
      | Sys_error(_) => agent_state("exited")
      };
    | None => ()
    };
    Mutex.unlock(agent_m);
  };
};

let stop_agent = () => {
  Mutex.lock(agent_m);
  switch (agent_pid^) {
  | Some(pid) =>
    try(Unix.kill(pid, Sys.sigterm)) {
    | Unix.Unix_error(_) => ()
    }
  | None => ()
  };
  Mutex.unlock(agent_m);
};

/* ---------- http ---------- */

let url_decode = s => {
  let b = Buffer.create(String.length(s));
  let i = ref(0);
  while (i^ < String.length(s)) {
    switch (s.[i^]) {
    | '%' when i^ + 2 < String.length(s) =>
      try(
        {
          Buffer.add_char(
            b,
            Char.chr(int_of_string("0x" ++ String.sub(s, i^ + 1, 2))),
          );
          i := i^ + 2;
        }
      ) {
      | _ => Buffer.add_char(b, '%')
      }
    | '+' => Buffer.add_char(b, ' ')
    | c => Buffer.add_char(b, c)
    };
    incr(i);
  };
  Buffer.contents(b);
};

let parse_target = target =>
  switch (String.index_opt(target, '?')) {
  | None => (url_decode(target), [])
  | Some(i) =>
    let q = String.sub(target, i + 1, String.length(target) - i - 1);
    (
      url_decode(String.sub(target, 0, i)),
      List.filter_map(
        kv =>
          switch (String.index_opt(kv, '=')) {
          | Some(j) =>
            Some((
              url_decode(String.sub(kv, 0, j)),
              url_decode(String.sub(kv, j + 1, String.length(kv) - j - 1)),
            ))
          | None => None
          },
        String.split_on_char('&', q),
      ),
    );
  };

let ctype = path => {
  let has = Filename.check_suffix(path);
  if (has(".html")) {
    "text/html; charset=utf-8";
  } else if (has(".js")) {
    "text/javascript; charset=utf-8";
  } else if (has(".css")) {
    "text/css; charset=utf-8";
  } else if (has(".json") || has(".jsonl")) {
    "application/json; charset=utf-8";
  } else if (has(".svg")) {
    "image/svg+xml";
  } else if (has(".woff2")) {
    "font/woff2";
  } else {
    "text/plain; charset=utf-8";
  };
};

let respond = (oc, ~status="200 OK", ~ctype="text/plain; charset=utf-8", body) => {
  Printf.fprintf(
    oc,
    "HTTP/1.1 %s\r\nContent-Type: %s\r\nContent-Length: %d\r\nCache-Control: no-store\r\nConnection: close\r\n\r\n%s",
    status,
    ctype,
    String.length(body),
    body,
  );
  flush(oc);
};

let safe = p => !List.mem("..", String.split_on_char('/', p));

let serve_file = (oc, path) =>
  if (safe(path) && Sys.file_exists(path) && !Sys.is_directory(path)) {
    respond(oc, ~ctype=ctype(path), read_file(path));
  } else {
    respond(oc, ~status="404 Not Found", "not found");
  };

let sse_headers = oc => {
  output_string(
    oc,
    "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\nCache-Control: no-cache\r\nConnection: keep-alive\r\n\r\n",
  );
  flush(oc);
};

let sse = (oc, s) => {
  output_string(oc, "data: ");
  output_string(oc, s);
  output_string(oc, "\n\n");
};

/* Live: everything so far at once, then a marker, then events as they come. */
let events_live = oc => {
  sse_headers(oc);
  let cursor = ref(0)
  and first = ref(true);
  while (true) {
    let batch = Bus.wait_from(cursor^);
    List.iter(sse(oc), batch);
    cursor := cursor^ + List.length(batch);
    if (first^) {
      sse(oc, "{\"type\":\"caught_up\"}");
      first := false;
    };
    flush(oc);
  };
};

/* Replay: a recorded trace, with its own timing, from the start for each viewer. */
let replay_lines: ref(list(string)) = (ref([]): ref(list(string)));
let replay_speed = ref(1.0);

let events_replay = oc => {
  sse_headers(oc);
  sse(oc, "{\"type\":\"caught_up\"}");
  flush(oc);
  let prev = ref(0);
  List.iter(
    line => {
      let t =
        try(
          Yojson.Safe.Util.(
            Yojson.Safe.from_string(line) |> member("t") |> to_int
          )
        ) {
        | _ => prev^
        };
      let gap =
        Float.min(2.0, float_of_int(t - prev^) /. 1000. /. replay_speed^);
      if (gap > 0.005) {
        Thread.delay(gap);
      };
      prev := t;
      sse(oc, line);
      flush(oc);
    },
    replay_lines^,
  );
  sse(oc, "{\"type\":\"replay_end\"}");
  flush(oc);
  while (true) {
    Thread.delay(30.);
    output_string(oc, ": keep\n\n");
    flush(oc);
  };
};

let replaying = ref(false);

let handle = fd => {
  let ic = Unix.in_channel_of_descr(fd)
  and oc = Unix.out_channel_of_descr(fd);
  try({
    let line = String.trim(input_line(ic));
    let (meth, target) =
      switch (String.split_on_char(' ', line)) {
      | [m, t, ..._] => (m, t)
      | _ => ("", "/")
      };
    let rec headers = acc =>
      switch (String.trim(input_line(ic))) {
      | "" => acc
      | l =>
        switch (String.index_opt(l, ':')) {
        | Some(i) =>
          headers([
            (
              String.lowercase_ascii(String.sub(l, 0, i)),
              String.trim(String.sub(l, i + 1, String.length(l) - i - 1)),
            ),
            ...acc,
          ])
        | None => headers(acc)
        }
      };

    let hs = headers([]);
    let body =
      switch (List.assoc_opt("content-length", hs)) {
      | Some(n) => really_input_string(ic, int_of_string(n))
      | None => ""
      };
    let (path, query) = parse_target(target);
    switch (meth, path) {
    | ("GET", "/events") =>
      if (replaying^) {
        events_replay(oc);
      } else {
        events_live(oc);
      }
    | ("GET", "/" | "/index.html") =>
      serve_file(oc, home^ / "web" / "index.html")
    | ("GET", "/trace") =>
      respond(
        oc,
        ~ctype="application/json; charset=utf-8",
        String.concat(
          "\n",
          if (replaying^) {
            replay_lines^;
          } else {
            Bus.all();
          },
        ),
      )
    | ("GET", "/source") =>
      let file =
        try(List.assoc("file", query)) {
        | Not_found => ""
        };
      let p = project^ / file;
      if (project^ != "" && safe(file) && Sys.file_exists(p)) {
        respond(oc, read_file(p));
      } else {
        respond(oc, ~status="404 Not Found", "not found");
      };
    | ("POST", "/prompt") when ! replaying^ =>
      let text =
        try(
          Yojson.Safe.Util.(
            Yojson.Safe.from_string(body) |> member("text") |> to_string
          )
        ) {
        | _ => body
        };
      if (String.trim(text) != "") {
        send_prompt(text);
      };
      respond(oc, ~ctype="application/json", "{\"ok\":true}");
    | ("POST", "/stop") when ! replaying^ =>
      stop_agent();
      respond(oc, ~ctype="application/json", "{\"ok\":true}");
    | ("GET", p)
        when String.length(p) > 8 && String.sub(p, 0, 8) == "/canvas/" =>
      serve_file(
        oc,
        home^ / "canvas" / "dist" / String.sub(p, 8, String.length(p) - 8),
      )
    | ("GET", p) =>
      serve_file(oc, home^ / "web" / String.sub(p, 1, String.length(p) - 1))
    | _ => respond(oc, ~status="405 Method Not Allowed", "no")
    };
  }) {
  | _ => ()
  };
  try(flush(oc)) {
  | _ => ()
  };
  try(Unix.close(fd)) {
  | _ => ()
  };
};

let serve = () => {
  let sock = Unix.socket(Unix.PF_INET, Unix.SOCK_STREAM, 0);
  Unix.setsockopt(sock, Unix.SO_REUSEADDR, true);
  Unix.bind(
    sock,
    [@implicit_arity] Unix.ADDR_INET(Unix.inet_addr_loopback, port^),
  );
  Unix.listen(sock, 64);
  Printf.printf("Portolan: http://127.0.0.1:%d/\n%!", port^);
  while (true) {
    switch (Unix.accept(sock)) {
    | (fd, _) => ignore(Thread.create(handle, fd))
    | exception ([@implicit_arity] Unix.Unix_error(Unix.EINTR, _, _)) => ()
    };
  };
};

/* ---------- entry ---------- */

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

let absolute = p =>
  if (Filename.is_relative(p)) {
    Sys.getcwd() / p;
  } else {
    p;
  };

let () = {
  Sys.set_signal(Sys.sigpipe, Sys.Signal_ignore);
  let args = Array.to_list(Sys.argv) |> List.tl;
  let rec flags =
    fun
    | ["--port", n, ...r] => {
        port := int_of_string(n);
        flags(r);
      }
    | ["--home", h, ...r] => {
        home := absolute(h);
        flags(r);
      }
    | ["--agent", a, ...r] => {
        agent_kind := a;
        flags(r);
      }
    | ["--model", m, ...r] => {
        model := m;
        flags(r);
      }
    | ["--nudge", f, ...r] => {
        prompt_extra := absolute(f);
        flags(r);
      }
    | ["--extract", e, ...r] => {
        extract_exe := absolute(e);
        flags(r);
      }
    | ["--speed", s, ...r] => {
        replay_speed := float_of_string(s);
        flags(r);
      }
    | [x, ...r] => [x, ...flags(r)]
    | [] => [];

  let positional = flags(args);
  if (home^ == "") {
    home := find_home();
  };
  if (extract_exe^ == "") {
    extract_exe := home^ / "_build" / "default" / "extract" / "main.exe";
  };
  switch (positional) {
  | ["serve", dir] =>
    project := absolute(dir);
    if (!Sys.file_exists(project^)) {
      /* A new project gets a workspace file, so dune treats it as its own
         root even when it sits inside another dune project. */
      mkdir_p(project^);
      write_file(project^ / "dune-workspace", "(lang dune 3.16)\n");
    };
    project := Unix.realpath(project^);
    mkdir_p(project^ / ".portolan");
    let stamp = {
      let tm = Unix.localtime(now());
      Printf.sprintf(
        "%04d%02d%02d-%02d%02d%02d",
        tm.tm_year + 1900,
        tm.tm_mon + 1,
        tm.tm_mday,
        tm.tm_hour,
        tm.tm_min,
        tm.tm_sec,
      );
    };
    Bus.trace :=
      Some(
        open_out(project^ / ".portolan" / ("trace-" ++ stamp ++ ".jsonl")),
      );
    Bus.emit([
      ("type", `String("hello")),
      ("project", `String(Filename.basename(project^))),
      ("agent", `String(agent_kind^)),
      ("mode", `String("live")),
    ]);
    ignore(Thread.create(watcher, ()));
    serve();
  | ["replay", file] =>
    replaying := true;
    replay_lines :=
      List.filter(
        l => String.trim(l) != "",
        String.split_on_char('\n', read_file(absolute(file))),
      );
    serve();
  | _ =>
    prerr_endline(
      "usage:\n  portolan serve <project-dir> [--port N] [--agent claude|script:FILE] [--model M] [--nudge FILE]\n  portolan replay <trace.jsonl> [--port N] [--speed X]",
    );
    exit(2);
  };
};
