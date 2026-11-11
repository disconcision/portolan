let (/) = Filename.concat;

type t = {
  file: string,
  root: string,
  turn: Turn.t,
  /* Guards the fields below. Not held while calling [Turn]. */
  m: Mutex.t,
  /* Signals that one of them changed. */
  wake: Condition.t,
  /* Which play is the current one, and whether a stop was asked of it. */
  mutable play: int,
  mutable halt: bool,
  answered: Hashtbl.t(string, Event.answer),
};

let make = (~file, project, turn) => {
  file,
  root: Project.root(project),
  turn,
  m: Mutex.create(),
  wake: Condition.create(),
  play: 0,
  halt: false,
  answered: Hashtbl.create(4),
};

let locked = (s, f) => {
  Mutex.lock(s.m);
  Fun.protect(~finally=() => Mutex.unlock(s.m), f);
};

/* ---------- steps ---------- */

type step =
  | Say(string)
  | Wait(int)
  | Write({
      file: string,
      content: string,
    })
  | Edit({
      file: string,
      old: string,
      by: string,
    })
  | Ask({
      tool: string,
      input: Yojson.Safe.t,
    });

let step_of_json = (json: Yojson.Safe.t) =>
  Yojson.Safe.Util.(
    switch (json) {
    | `Assoc([("say", `String(text))]) => Some(Say(text))
    | `Assoc([("wait", `Int(ms))]) => Some(Wait(ms))
    | `Assoc([("write", w)]) =>
      Some(
        Write({
          file: w |> member("file") |> to_string,
          content: w |> member("content") |> to_string,
        }),
      )
    | `Assoc([("edit", e)]) =>
      Some(
        Edit({
          file: e |> member("file") |> to_string,
          old: e |> member("old") |> to_string,
          by: e |> member("new") |> to_string,
        }),
      )
    | `Assoc([("ask", r)]) =>
      Some(
        Ask({
          tool: r |> member("tool") |> to_string,
          input: r |> member("input"),
        }),
      )
    | _ => None
    }
  );

let replace = (~old, ~by, text) =>
  switch (Str.search_forward(Str.regexp_string(old), text, 0)) {
  | i =>
    let after = i + String.length(old);
    Some(
      String.sub(text, 0, i)
      ++ by
      ++ String.sub(text, after, String.length(text) - after),
    );
  | exception Not_found => None
  };

/* What the agent is told of the user's answer, and whether it is an error. */
let outcome = (answer: Event.answer) =>
  switch (answer) {
  | Deny => (true, "The user denied this.")
  | Answers(answers) => (
      false,
      String.concat(
        "; ",
        List.map(((q, answer)) => q ++ " = " ++ answer, answers),
      ),
    )
  | Once
  | Always(_) => (false, "Allowed.")
  };

/* ---------- playing ---------- */

exception Stopped;

let run = s => {
  let mine =
    locked(
      s,
      () => {
        s.play = s.play + 1;
        s.halt = false;
        s.play;
      },
    );
  Turn.happened(s.turn, Began);
  /* Ends the play when it was stopped; false once it has been forgotten. */
  let live = () =>
    locked(s, () =>
      if (s.play != mine) {
        false;
      } else if (s.halt) {
        raise(Stopped);
      } else {
        true;
      }
    );
  let str = text => `String(text);
  let message = (role, blocks) =>
    if (live()) {
      Turn.happened(
        s.turn,
        Told(
          `Assoc([
            ("type", str(role)),
            (
              "message",
              `Assoc([("role", str(role)), ("content", `List(blocks))]),
            ),
          ]),
        ),
      );
    };
  let uses = ref(0);
  let tool = (name, input) => {
    incr(uses);
    let id = Printf.sprintf("script_%d_%d", mine, uses^);
    message(
      "assistant",
      [
        `Assoc([
          ("type", str("tool_use")),
          ("id", str(id)),
          ("name", str(name)),
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
          ("type", str("tool_result")),
          ("tool_use_id", str(id)),
          ("content", str(text)),
          ("is_error", `Bool(error)),
        ]),
      ],
    );
  let abs = f => Filename.is_relative(f) ? s.root / f : f;
  let step =
    fun
    | Say(text) =>
      message(
        "assistant",
        [`Assoc([("type", str("text")), ("text", str(text))])],
      )
    | Wait(ms) =>
      /* In short naps, so a stop is noticed. */
      for (_ in 1 to max(1, Int.div(ms, 100))) {
        if (live()) {
          Thread.delay(0.1);
        };
      }
    | Write({file, content}) => {
        let file = abs(file);
        let id =
          tool(
            "Write",
            `Assoc([("file_path", str(file)), ("content", str(content))]),
          );
        Files.mkdir_p(Filename.dirname(file));
        Files.write(file, content);
        result(id, "File written.");
      }
    | Edit({file, old, by}) => {
        let file = abs(file);
        let id =
          tool(
            "Edit",
            `Assoc([
              ("file_path", str(file)),
              ("old_string", str(old)),
              ("new_string", str(by)),
            ]),
          );
        switch (replace(~old, ~by, Files.read(file))) {
        | Some(text) =>
          Files.write(file, text);
          result(id, "Edited.");
        | None => result(~error=true, id, "Error: old text not found.")
        };
      }
    | Ask({tool: name, input}) => {
        let id = tool(name, input);
        Turn.happened(
          s.turn,
          Asked({
            id,
            tool: name,
            input,
            suggestions: `Null,
            tool_use: Some(id),
          }),
        );
        let answer =
          locked(
            s,
            () => {
              while (s.play == mine && !s.halt && !Hashtbl.mem(s.answered, id)) {
                Condition.wait(s.wake, s.m);
              };
              Hashtbl.find_opt(s.answered, id);
            },
          );
        switch (answer) {
        | Some(answer) =>
          let (error, text) = outcome(answer);
          result(~error, id, text);
        | None => ignore(live())
        };
      };
  let finish = error =>
    if (locked(s, () => s.play == mine)) {
      Turn.happened(
        s.turn,
        Ended(
          `Assoc([
            ("type", str("result")),
            ("subtype", str(error ? "error_during_execution" : "success")),
            ("is_error", `Bool(error)),
          ]),
        ),
      );
    };
  switch (
    Yojson.Safe.from_file(s.file)
    |> Yojson.Safe.Util.member("steps")
    |> Yojson.Safe.Util.to_list
    |> List.iter(json =>
         if (live()) {
           Option.iter(step, step_of_json(json));
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

let play = s => ignore(Thread.create(run, s));

let answer = (s, ~id, answer) =>
  locked(
    s,
    () => {
      Hashtbl.replace(s.answered, id, answer);
      Condition.broadcast(s.wake);
    },
  );

let stop = s =>
  locked(
    s,
    () => {
      s.halt = true;
      Condition.broadcast(s.wake);
    },
  );

let forget = s =>
  locked(
    s,
    () => {
      s.play = s.play + 1;
      Hashtbl.reset(s.answered);
      Condition.broadcast(s.wake);
    },
  );
