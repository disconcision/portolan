let (/) = Filename.concat;

type source =
  | Live(Project.t, Agent.t)
  | Replay({
      lines: list(string),
      speed: float,
    });

/* ---------- requests ---------- */

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

/* "/path?a=1&b=2" -> ("/path", [("a", "1"), ("b", "2")]) */
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

type request = {
  meth: string,
  path: string,
  query: list((string, string)),
  body: string,
};

let read_request = ic => {
  let (meth, target) =
    switch (String.split_on_char(' ', String.trim(input_line(ic)))) {
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
  let body =
    switch (List.assoc_opt("content-length", headers([]))) {
    | Some(n) => really_input_string(ic, int_of_string(n))
    | None => ""
    };
  let (path, query) = parse_target(target);
  {
    meth,
    path,
    query,
    body,
  };
};

/* ---------- responses ---------- */

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

let respond =
    (
      oc,
      ~status="200 OK",
      ~ctype="text/plain; charset=utf-8",
      ~headers=[],
      body,
    ) => {
  Printf.fprintf(
    oc,
    "HTTP/1.1 %s\r\nContent-Type: %s\r\nContent-Length: %d\r\nCache-Control: no-store\r\nConnection: close\r\n%s\r\n%s",
    status,
    ctype,
    String.length(body),
    String.concat(
      "",
      List.map(((name, value)) => name ++ ": " ++ value ++ "\r\n", headers),
    ),
    body,
  );
  flush(oc);
};

let not_found = oc => respond(oc, ~status="404 Not Found", "not found");
let ok = oc => respond(oc, ~ctype="application/json", "{\"ok\":true}");

let serve_file = (oc, path) =>
  if (!List.mem("..", String.split_on_char('/', path))
      && Sys.file_exists(path)
      && !Sys.is_directory(path)) {
    respond(oc, ~ctype=ctype(path), Files.read(path));
  } else {
    not_found(oc);
  };

/* ---------- the event stream ---------- */

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

/* Live: the trace so far at once, then a marker, then events as they
   come. A viewer joining late needs only the latest graph, so the
   backlog's graph events are sent as one. If the trace is started over,
   the same again from its start. */
let events_live = oc => {
  sse_headers(oc);
  let cursor = ref(Bus.origin);
  while (true) {
    let (batch, next, from_start) = Bus.wait(cursor^);
    cursor := next;
    if (from_start) {
      sse(oc, "{\"type\":\"reset\"}");
      List.iter(sse(oc), Graphs.collapse(batch));
      sse(oc, "{\"type\":\"caught_up\"}");
    } else {
      List.iter(sse(oc), batch);
    };
    flush(oc);
  };
};

/* Replay: a recorded trace, with its own timing, from the start for each viewer. */
let events_replay = (oc, ~lines, ~speed) => {
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
      let gap = Float.min(2.0, float_of_int(t - prev^) /. 1000. /. speed);
      if (gap > 0.005) {
        Thread.delay(gap);
      };
      prev := t;
      sse(oc, line);
      flush(oc);
    },
    lines,
  );
  sse(oc, "{\"type\":\"replay_end\"}");
  flush(oc);
  while (true) {
    Thread.delay(30.);
    output_string(oc, ": keep\n\n");
    flush(oc);
  };
};

/* ---------- routes ---------- */

let refmt = (oc, project, query) => {
  let param = name => List.assoc_opt(name, query);
  let number = name => Option.bind(param(name), int_of_string_opt);
  switch (Option.bind(param("file"), Project.file(project))) {
  | None => not_found(oc)
  | Some(path) =>
    let first = Option.value(number("line"), ~default=1)
    and last = number("end_line");
    let (text, converted) =
      switch (Option.bind(param("to"), Syntax.of_string)) {
      | Some(target) => Syntax.convert(~path, ~first, ~last, ~target)
      | None => (Syntax.slice(~first, ~last, Files.read(path)), false)
      };
    respond(
      oc,
      ~headers=converted ? [] : [("X-Portolan-Converted", "no")],
      text,
    );
  };
};

/* The definition a prompt is about, from the page's JSON. */
let about = json =>
  switch (json) {
  | `Assoc(fields) =>
    switch (List.assoc_opt("name", fields)) {
    | Some(`String(name)) =>
      Some({
        Agent.name,
        where:
          switch (
            List.assoc_opt("file", fields),
            List.assoc_opt("line", fields),
            List.assoc_opt("end_line", fields),
          ) {
          | (Some(`String(file)), Some(`Int(first)), Some(`Int(last))) =>
            Some((file, first, last))
          | _ => None
          },
      })
    | _ => None
    }
  | _ => None
  };

/* The user's answer to a request of the agent's, from the page's JSON. */
let answer = json => {
  open Yojson.Safe.Util;
  let strings = name =>
    switch (json |> member(name)) {
    | `List(items) => List.filter_map(to_string_option, items)
    | _ => []
    };
  let answer =
    switch (json |> member("decision")) {
    | `String("once") => Some(Agent.Once)
    | `String("always") => Some(Agent.Always(strings("patterns")))
    | `String("deny") => Some(Agent.Deny)
    | `String("answered") =>
      Some(
        Agent.Answers(
          json
          |> member("answers")
          |> to_assoc
          |> List.map(((q, label)) => (q, to_string(label))),
        ),
      )
    | _ => None
    };
  switch (json |> member("id"), answer) {
  | (`String(id), Some(answer)) => Some((id, answer))
  | _ => None
  };
};

let route = (oc, ~home, source, {meth, path, query, body}) =>
  switch (meth, path, source) {
  | ("GET", "/events", Live(_)) => events_live(oc)
  | ("GET", "/events", Replay({lines, speed})) =>
    events_replay(oc, ~lines, ~speed)
  | ("GET", "/" | "/index.html", _) =>
    serve_file(oc, home / "web" / "index.html")
  | ("GET", "/trace", _) =>
    respond(
      oc,
      ~ctype="application/json; charset=utf-8",
      String.concat(
        "\n",
        switch (source) {
        | Live(_) => Bus.all()
        | Replay({lines, _}) => lines
        },
      ),
    )
  | ("GET", "/source", Live(project, _)) =>
    switch (
      Option.bind(List.assoc_opt("file", query), Project.file(project))
    ) {
    | Some(file) => respond(oc, Files.read(file))
    | None => not_found(oc)
    }
  | ("GET", "/source" | "/refmt", Replay(_)) => not_found(oc)
  | ("GET", "/refmt", Live(project, _)) => refmt(oc, project, query)
  | ("POST", "/prompt", Live(_, agent)) =>
    let (text, about) =
      switch (Yojson.Safe.from_string(body)) {
      | json => (
          Yojson.Safe.Util.(json |> member("text") |> to_string),
          about(Yojson.Safe.Util.member("about", json)),
        )
      | exception _ => (body, None)
      };
    if (String.trim(text) != "") {
      Agent.prompt(agent, ~about?, text);
    };
    ok(oc);
  | ("POST", "/answer", Live(_, agent)) =>
    switch (answer(Yojson.Safe.from_string(body))) {
    | Some((id, answer)) =>
      Agent.answer(agent, ~id, answer);
      ok(oc);
    | None
    | exception _ => respond(oc, ~status="400 Bad Request", "no")
    }
  | ("POST", "/new", Live(project, agent)) =>
    Conversation.start_new(project, agent);
    ok(oc);
  | ("POST", "/types", Live(_)) =>
    let texts =
      try(
        Yojson.Safe.Util.(
          Yojson.Safe.from_string(body)
          |> member("types")
          |> to_list
          |> List.map(to_string)
        )
      ) {
      | _ => []
      };
    let answer =
      Syntax.types(texts)
      |> List.map(
           fun
           | Some(t) => `String(t)
           | None => `Null,
         );
    respond(
      oc,
      ~ctype="application/json; charset=utf-8",
      Yojson.Safe.to_string(`Assoc([("types", `List(answer))])),
    );
  | ("POST", "/stop", Live(_, agent)) =>
    Agent.stop(agent);
    ok(oc);
  | ("GET", p, _) when String.starts_with(~prefix="/canvas/", p) =>
    serve_file(
      oc,
      home / "canvas" / "dist" / String.sub(p, 8, String.length(p) - 8),
    )
  | ("GET", p, _) =>
    serve_file(oc, home / "web" / String.sub(p, 1, String.length(p) - 1))
  | _ => respond(oc, ~status="405 Method Not Allowed", "no")
  };

let handle = (~home, source, fd) => {
  let ic = Unix.in_channel_of_descr(fd)
  and oc = Unix.out_channel_of_descr(fd);
  try(route(oc, ~home, source, read_request(ic))) {
  | _ =>
    /* Usually the viewer has gone; if not, say that it went wrong. */
    try(respond(oc, ~status="500 Internal Server Error", "error")) {
    | _ => ()
    }
  };
  try(flush(oc)) {
  | _ => ()
  };
  try(Unix.close(fd)) {
  | _ => ()
  };
};

let serve = (~port, ~home, source) => {
  let sock = Unix.socket(Unix.PF_INET, Unix.SOCK_STREAM, 0);
  Unix.setsockopt(sock, Unix.SO_REUSEADDR, true);
  Unix.bind(sock, Unix.ADDR_INET(Unix.inet_addr_loopback, port));
  Unix.listen(sock, 64);
  Printf.printf("Portolan: http://127.0.0.1:%d/\n%!", port);
  while (true) {
    switch (Unix.accept(sock)) {
    | (fd, _) => ignore(Thread.create(handle(~home, source), fd))
    | exception (Unix.Unix_error(Unix.EINTR, _, _)) => ()
    };
  };
};
