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

/* What does the work. */
type worker =
  | Claude_code(Claude.t)
  | Scripted(Script.t);

type t = {
  project: Project.t,
  turn: Turn.t,
  worker,
};

let make = (~kind, ~model=?, ~instructions, ~allow, project) => {
  let turn = Turn.make(project);
  {
    project,
    turn,
    worker:
      switch (kind) {
      | Claude =>
        Claude_code(
          Claude.make(~model?, ~instructions, ~allow, project, turn),
        )
      | Script(file) => Scripted(Script.make(~file, project, turn))
      },
  };
};

/* What the agent is told about the definition a prompt is about. */
let about_text = (about: Event.about) =>
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
  switch (a.worker) {
  | Scripted(script) => Script.play(script)
  | Claude_code(claude) =>
    Claude.prompt(
      claude,
      text ++ Option.fold(~none="", ~some=about_text, about),
    )
  };
};

let answer = (a, ~id, answer: Event.answer) =>
  switch (Turn.answered(a.turn, ~id, answer)) {
  | None => ()
  | Some(input) =>
    switch (answer) {
    | Always(patterns) => Permissions.remember(a.project, patterns)
    | Once
    | Deny
    | Answers(_) => ()
    };
    switch (a.worker) {
    | Claude_code(claude) => Claude.answer(claude, ~id, ~input, answer)
    | Scripted(script) => Script.answer(script, ~id, answer)
    };
  };

let stop = a =>
  switch (a.worker) {
  | Claude_code(claude) => Claude.stop(claude)
  | Scripted(script) => Script.stop(script)
  };

let forget = a => {
  switch (a.worker) {
  | Claude_code(claude) => Claude.forget(claude)
  | Scripted(script) => Script.forget(script)
  };
  Turn.happened(a.turn, Forgotten);
};

let take_cause = a => Turn.take_cause(a.turn);
