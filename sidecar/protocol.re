type request = {
  id: string,
  tool: string,
  input: Yojson.Safe.t,
  suggestions: Yojson.Safe.t,
  tool_use: option(string),
};

type said =
  | Began(Yojson.Safe.t, option(string))
  | Ended(Yojson.Safe.t)
  | Told(Yojson.Safe.t)
  | Asked(request)
  | Asked_other(string)
  | Withdrew(string)
  | Nothing
  | Raw(string);

/* ---------- reading ---------- */

let control_request = ev => {
  open Yojson.Safe.Util;
  let id = ev |> member("request_id") |> to_string;
  let request = ev |> member("request");
  switch (request |> member("subtype")) {
  | `String("can_use_tool") =>
    Asked({
      id,
      tool: request |> member("tool_name") |> to_string,
      input: request |> member("input"),
      suggestions: request |> member("permission_suggestions"),
      tool_use: request |> member("tool_use_id") |> to_string_option,
    })
  | _ => Asked_other(id)
  };
};

let read = line =>
  switch (Yojson.Safe.from_string(line)) {
  | `Assoc(fields) as ev =>
    let field = name =>
      Option.value(List.assoc_opt(name, fields), ~default=`Null);
    switch (field("type"), field("subtype")) {
    | (`String("control_request"), _) =>
      try(control_request(ev)) {
      | _ => Nothing
      }
    | (`String("control_cancel_request"), _) =>
      switch (field("request_id")) {
      | `String(id) => Withdrew(id)
      | _ => Nothing
      }
    | (`String("control_response"), _) => Nothing
    | (`String("system"), `String("init")) =>
      Began(
        ev,
        switch (field("session_id")) {
        | `String(id) => Some(id)
        | _ => None
        },
      )
    | (`String("result"), _) => Ended(ev)
    | _ => Told(ev)
    };
  | _
  | exception _ => String.trim(line) == "" ? Nothing : Raw(line)
  };

let cause = (~relative, ev) => {
  open Yojson.Safe.Util;
  let found = ref(None);
  /* A block that is not as expected ends the look; what was found stands. */
  try(
    if (ev |> member("type") |> to_string == "assistant") {
      ev
      |> member("message")
      |> member("content")
      |> to_list
      |> List.iter(block =>
           if (block |> member("type") |> to_string == "tool_use") {
             let name = block |> member("name") |> to_string;
             let input = block |> member("input");
             let text = key =>
               try(input |> member(key) |> to_string) {
               | _ => ""
               };
             switch (name) {
             | "Write"
             | "Edit"
             | "MultiEdit" =>
               let file = text("file_path");
               found :=
                 Some(
                   Event.Wrote({
                     tool: name,
                     file: Option.value(relative(file), ~default=file),
                   }),
                 );
             | "Bash" =>
               found := Some(Ran(Event.clip(~n=120, text("command"))))
             | _ => ()
             };
           }
         );
    }
  ) {
  | _ => ()
  };
  found^;
};

/* ---------- writing ---------- */

type message = Yojson.Safe.t;

let to_string = message => Yojson.Safe.to_string(message);

let str = s => `String(s);

let prompt = text =>
  `Assoc([
    ("type", str("user")),
    ("message", `Assoc([("role", str("user")), ("content", str(text))])),
  ]);

let allow = (~rules=[], input) =>
  `Assoc(
    [("behavior", str("allow")), ("updatedInput", input)]
    @ (
      rules == []
        ? []
        : [
          (
            "updatedPermissions",
            `List([
              `Assoc([
                ("type", str("addRules")),
                ("rules", `List(List.map(Permissions.rule, rules))),
                ("behavior", str("allow")),
                /* For the running process; the next one is started with
                   the remembered patterns. */
                ("destination", str("session")),
              ]),
            ]),
          ),
        ]
    ),
  );

let decision = (~input, answer: Event.answer) =>
  switch (answer) {
  | Once => allow(input)
  | Always(patterns) => allow(~rules=patterns, input)
  | Deny =>
    `Assoc([
      ("behavior", str("deny")),
      (
        "message",
        str(
          "The user declined this in Portolan's chat. Do not retry it; carry on another way or ask what they would prefer.",
        ),
      ),
    ])
  | Answers(answers) =>
    let answers =
      `Assoc(List.map(((q, label)) => (q, str(label)), answers));
    allow(
      switch (input) {
      | `Assoc(fields) =>
        `Assoc(
          List.remove_assoc("answers", fields) @ [("answers", answers)],
        )
      | other => other
      },
    );
  };

let response = fields =>
  `Assoc([
    ("type", str("control_response")),
    ("response", `Assoc(fields)),
  ]);

let answer = (~id, ~input, answer) =>
  response([
    ("subtype", str("success")),
    ("request_id", str(id)),
    ("response", decision(~input, answer)),
  ]);

let refusal = (~id) =>
  response([
    ("subtype", str("error")),
    ("request_id", str(id)),
    ("error", str("Portolan does not handle this request.")),
  ]);

let interrupt = (~id) =>
  `Assoc([
    ("type", str("control_request")),
    ("request_id", str(id)),
    ("request", `Assoc([("subtype", str("interrupt"))])),
  ]);
