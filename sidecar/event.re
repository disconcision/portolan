type state =
  | Running
  | Waiting
  | Idle
  | Exited;

type about = {
  name: string,
  where: option((string, int, int)),
};

type cause =
  | By_hand
  | Wrote({
      tool: string,
      file: string,
    })
  | Ran(string)
  | Tested
  | Carried_over;

type drawing =
  | Whole(Yojson.Safe.t)
  | Change(Yojson.Safe.t);

type answer =
  | Once
  | Always(list(string))
  | Deny
  | Answers(list((string, string)));

type t =
  | Session({restart: bool})
  | Hello({
      project: string,
      agent: string,
      opam_switch: option(string),
    })
  | Conversation({earlier: string})
  | Build({
      ok: bool,
      output: string,
      ms: int,
    })
  | Graph({
      drawing,
      cause,
      partial: bool,
    })
  | Tests({
      ok: bool,
      output: string,
      failing: list((string, int)),
    })
  | Extract_failed(string)
  | Error({
      where: string,
      message: string,
    })
  | Prompt({
      text: string,
      about: option(about),
    })
  | Agent(Yojson.Safe.t)
  | Agent_state(state)
  | Agent_raw(string)
  | Ask({
      id: string,
      tool: string,
      input: Yojson.Safe.t,
      patterns: list(string),
      tool_use: option(string),
    })
  | Answer({
      id: string,
      answer: option(answer),
    });

module Kind = {
  type t =
    | Session
    | Hello
    | Conversation
    | Build
    | Graph
    | Tests
    | Extract
    | Error
    | Prompt
    | Agent
    | Agent_state
    | Agent_raw
    | Ask
    | Answer;

  let names = [
    (Session, "session"),
    (Hello, "hello"),
    (Conversation, "conversation"),
    (Build, "build"),
    (Graph, "graph"),
    (Tests, "tests"),
    (Extract, "extract"),
    (Error, "error"),
    (Prompt, "prompt"),
    (Agent, "agent"),
    (Agent_state, "agent_state"),
    (Agent_raw, "agent_raw"),
    (Ask, "ask"),
    (Answer, "answer"),
  ];

  let name = kind => List.assoc(kind, names);

  let of_name = name =>
    List.find_map(((kind, n)) => n == name ? Some(kind) : None, names);
};

let kind =
  fun
  | Session(_) => Kind.Session
  | Hello(_) => Kind.Hello
  | Conversation(_) => Kind.Conversation
  | Build(_) => Kind.Build
  | Graph(_) => Kind.Graph
  | Tests(_) => Kind.Tests
  | Extract_failed(_) => Kind.Extract
  | Error(_) => Kind.Error
  | Prompt(_) => Kind.Prompt
  | Agent(_) => Kind.Agent
  | Agent_state(_) => Kind.Agent_state
  | Agent_raw(_) => Kind.Agent_raw
  | Ask(_) => Kind.Ask
  | Answer(_) => Kind.Answer;

let states = [
  (Running, "running"),
  (Waiting, "waiting"),
  (Idle, "idle"),
  (Exited, "exited"),
];

/* ---------- writing ---------- */

let str = s => `String(s);
let strs = l => `List(List.map(str, l));

let about_json = about =>
  `Assoc(
    [("name", str(about.name))]
    @ (
      switch (about.where) {
      | Some((file, first, last)) => [
          ("file", str(file)),
          ("line", `Int(first)),
          ("end_line", `Int(last)),
        ]
      | None => []
      }
    ),
  );

let cause_json = cause =>
  `Assoc(
    switch (cause) {
    | By_hand => [("label", str("edit")), ("by", str("hand"))]
    | Wrote({tool, file}) => [
        ("label", str(tool)),
        ("tool", str(tool)),
        ("file", str(file)),
        ("by", str("agent")),
      ]
    | Ran(command) => [
        ("label", str("Bash")),
        ("tool", str("Bash")),
        ("command", str(command)),
        ("by", str("agent")),
      ]
    | Tested => [("label", str("tests"))]
    | Carried_over => [("label", str("carried over"))]
    },
  );

let drawing_field =
  fun
  | Whole(graph) => ("graph", graph)
  | Change(change) => ("delta", change);

let answer_fields =
  fun
  | Some(Once) => [("decision", str("once"))]
  | Some(Always(patterns)) => [
      ("decision", str("always")),
      ("patterns", strs(patterns)),
    ]
  | Some(Deny) => [("decision", str("deny"))]
  | Some(Answers(answers)) => [
      ("decision", str("answered")),
      (
        "answers",
        `Assoc(List.map(((q, label)) => (q, str(label)), answers)),
      ),
    ]
  | None => [("decision", str("cancelled"))];

let optional = (name, value) =>
  switch (value) {
  | Some(s) => [(name, str(s))]
  | None => []
  };

/* The fields after ["type"], in the order the page and older traces have them. */
let fields =
  fun
  | Session({restart}) => [("restart", `Bool(restart))]
  | Hello({project, agent, opam_switch}) =>
    [
      ("project", str(project)),
      ("agent", str(agent)),
      ("mode", str("live")),
    ]
    @ optional("switch", opam_switch)
  | Conversation({earlier}) => [("earlier", str(earlier))]
  | Build({ok, output, ms}) => [
      ("ok", `Bool(ok)),
      ("output", str(output)),
      ("ms", `Int(ms)),
    ]
  | Graph({drawing, cause, partial}) =>
    [drawing_field(drawing), ("cause", cause_json(cause))]
    @ (partial ? [("partial", `Bool(true))] : [])
  | Tests({ok, output, failing}) => [
      ("ok", `Bool(ok)),
      ("output", str(output)),
      (
        "failing",
        `List(
          List.map(
            ((file, line)) =>
              `Assoc([("file", str(file)), ("line", `Int(line))]),
            failing,
          ),
        ),
      ),
    ]
  | Extract_failed(output) => [
      ("ok", `Bool(false)),
      ("output", str(output)),
    ]
  | Error({where, message}) => [
      ("where", str(where)),
      ("message", str(message)),
    ]
  | Prompt({text, about}) =>
    [("text", str(text))]
    @ (
      switch (about) {
      | Some(about) => [("about", about_json(about))]
      | None => []
      }
    )
  | Agent(ev) => [("ev", ev)]
  | Agent_state(state) => [("state", str(List.assoc(state, states)))]
  | Agent_raw(line) => [("line", str(line))]
  | Ask({id, tool, input, patterns, tool_use}) =>
    [
      ("id", str(id)),
      ("tool", str(tool)),
      ("input", input),
      ("patterns", strs(patterns)),
    ]
    @ optional("tool_use_id", tool_use)
  | Answer({id, answer}) => [("id", str(id)), ...answer_fields(answer)];

let to_json = event =>
  `Assoc([("type", str(Kind.name(kind(event)))), ...fields(event)]);

/* ---------- reading ---------- */

let ( let* ) = Option.bind;

let field = (name, json) =>
  switch (json) {
  | `Assoc(fields) => List.assoc_opt(name, fields)
  | _ => None
  };

let string = (name, json) =>
  switch (field(name, json)) {
  | Some(`String(s)) => Some(s)
  | _ => None
  };

let int = (name, json) =>
  switch (field(name, json)) {
  | Some(`Int(n)) => Some(n)
  | _ => None
  };

let bool = (name, json) =>
  switch (field(name, json)) {
  | Some(`Bool(b)) => Some(b)
  | _ => None
  };

/* The items of a list if every one reads; [None] if one does not. */
let all = (read, items) =>
  List.fold_right(
    (item, rest) => {
      let* rest = rest;
      let* item = read(item);
      Some([item, ...rest]);
    },
    items,
    Some([]),
  );

let about_of_json = json => {
  let* name = string("name", json);
  Some({
    name,
    where:
      switch (
        string("file", json),
        int("line", json),
        int("end_line", json),
      ) {
      | (Some(file), Some(first), Some(last)) => Some((file, first, last))
      | _ => None
      },
  });
};

let cause_of_json = json =>
  switch (
    string("label", json),
    string("by", json),
    string("tool", json),
    string("command", json),
    string("file", json),
  ) {
  | (Some("edit"), Some("hand"), None, None, None) => Some(By_hand)
  | (Some("tests"), None, None, None, None) => Some(Tested)
  | (Some("carried over"), None, None, None, None) => Some(Carried_over)
  | (Some("Bash"), Some("agent"), Some("Bash"), Some(command), None) =>
    Some(Ran(command))
  | (Some(label), Some("agent"), Some(tool), None, Some(file))
      when label == tool =>
    Some(
      Wrote({
        tool,
        file,
      }),
    )
  | _ => None
  };

let answer_of_json = json =>
  switch (string("decision", json)) {
  | Some("once") => Some(Once)
  | Some("always") =>
    Some(
      Always(
        switch (field("patterns", json)) {
        | Some(`List(items)) =>
          List.filter_map(
            fun
            | `String(s) => Some(s)
            | _ => None,
            items,
          )
        | _ => []
        },
      ),
    )
  | Some("deny") => Some(Deny)
  | Some("answered") =>
    switch (field("answers", json)) {
    | Some(`Assoc(answers)) =>
      let* answers =
        all(
          fun
          | (q, `String(label)) => Some((q, label))
          | _ => None,
          answers,
        );
      Some(Answers(answers));
    | _ => None
    }
  | _ => None
  };

let of_json = json => {
  let* name = string("type", json);
  let* kind = Kind.of_name(name);
  switch (kind) {
  | Session =>
    let* restart = bool("restart", json);
    Some(Session({restart: restart}));
  | Hello =>
    let* project = string("project", json);
    let* agent = string("agent", json);
    Some(
      Hello({
        project,
        agent,
        opam_switch: string("switch", json),
      }),
    );
  | Conversation =>
    let* earlier = string("earlier", json);
    Some(Conversation({earlier: earlier}));
  | Build =>
    let* ok = bool("ok", json);
    let* output = string("output", json);
    let* ms = int("ms", json);
    Some(
      Build({
        ok,
        output,
        ms,
      }),
    );
  | Graph =>
    let* drawing =
      switch (field("graph", json), field("delta", json)) {
      | (Some(graph), None) => Some(Whole(graph))
      | (None, Some(change)) => Some(Change(change))
      | _ => None
      };
    let* cause = Option.bind(field("cause", json), cause_of_json);
    Some(
      Graph({
        drawing,
        cause,
        partial: bool("partial", json) == Some(true),
      }),
    );
  | Tests =>
    let* ok = bool("ok", json);
    let* output = string("output", json);
    let* failing =
      switch (field("failing", json)) {
      | Some(`List(items)) =>
        all(
          item => {
            let* file = string("file", item);
            let* line = int("line", item);
            Some((file, line));
          },
          items,
        )
      | _ => None
      };
    Some(
      Tests({
        ok,
        output,
        failing,
      }),
    );
  | Extract =>
    let* output = string("output", json);
    Some(Extract_failed(output));
  | Error =>
    let* where = string("where", json);
    let* message = string("message", json);
    Some(
      Error({
        where,
        message,
      }),
    );
  | Prompt =>
    let* text = string("text", json);
    Some(
      Prompt({
        text,
        about: Option.bind(field("about", json), about_of_json),
      }),
    );
  | Agent =>
    let* ev = field("ev", json);
    Some(Agent(ev));
  | Agent_state =>
    let* name = string("state", json);
    let* state =
      List.find_map(((s, n)) => n == name ? Some(s) : None, states);
    Some(Agent_state(state));
  | Agent_raw =>
    let* line = string("line", json);
    Some(Agent_raw(line));
  | Ask =>
    let* id = string("id", json);
    let* tool = string("tool", json);
    let* input = field("input", json);
    let* patterns =
      switch (field("patterns", json)) {
      | Some(`List(items)) =>
        all(
          fun
          | `String(s) => Some(s)
          | _ => None,
          items,
        )
      | _ => None
      };
    Some(
      Ask({
        id,
        tool,
        input,
        patterns,
        tool_use: string("tool_use_id", json),
      }),
    );
  | Answer =>
    let* id = string("id", json);
    switch (string("decision", json)) {
    | Some("cancelled") =>
      Some(
        Answer({
          id,
          answer: None,
        }),
      )
    | _ =>
      let* answer = answer_of_json(json);
      Some(
        Answer({
          id,
          answer: Some(answer),
        }),
      );
    };
  };
};

let clip = (~n=6000, s) =>
  if (String.length(s) <= n) {
    s;
  } else {
    String.sub(s, 0, n) ++ "\n…";
  };

module Line = {
  type t = string;

  let make = (~time, event) =>
    switch (to_json(event)) {
    | `Assoc(fields) =>
      Yojson.Safe.to_string(`Assoc([("t", `Int(time)), ...fields]))
    };

  let of_string = s => s;
  let to_string = line => line;

  let is_whole = line =>
    String.starts_with(~prefix="{", line)
    && String.ends_with(~suffix="}", line);

  let kind = line =>
    try(
      Scanf.sscanf(line, "{\"t\":%d,\"type\":\"%[^\"]\"", (_, name) =>
        Kind.of_name(name)
      )
    ) {
    | _ => None
    };

  let fields = line =>
    switch (Yojson.Safe.from_string(line)) {
    | `Assoc(fields) => Some(fields)
    | _
    | exception _ => None
    };

  let time = line => {
    let* fields = fields(line);
    switch (List.assoc_opt("t", fields)) {
    | Some(`Int(t)) => Some(t)
    | _ => None
    };
  };

  let event = line => {
    let* fields = fields(line);
    of_json(`Assoc(fields));
  };

  let draws = line =>
    try(
      Scanf.sscanf(
        line, "{\"t\":%d,\"type\":\"graph\",\"%[^\"]\"", (_, field) =>
        Some(field == "delta" ? `Change : `Whole)
      )
    ) {
    | _ => None
    };

  let drawing = line => {
    let* fields = fields(line);
    switch (
      List.assoc_opt("graph", fields),
      List.assoc_opt("delta", fields),
    ) {
    | (Some(graph), _) => Some(Whole(graph))
    | (None, Some(change)) => Some(Change(change))
    | (None, None) => None
    };
  };

  let redraw = (line, drawing) =>
    switch (fields(line)) {
    | Some(fields) =>
      Yojson.Safe.to_string(
        `Assoc(
          List.map(
            ((name, value)) =>
              name == "graph" || name == "delta"
                ? drawing_field(drawing) : (name, value),
            fields,
          ),
        ),
      )
    | None => line
    };
};
