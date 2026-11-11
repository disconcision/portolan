type standing = {
  /* The state last reported. */
  state: Event.state,
  finished: int,
  /* Requests waiting for the user's answer, oldest first: id and the
     tool's input. */
  pending: list((string, Yojson.Safe.t)),
};

let idle = {
  state: Idle,
  finished: 0,
  pending: [],
};

type happening =
  | Began
  | Told(Yojson.Safe.t)
  | Ended(Yojson.Safe.t)
  | Asked(Protocol.request)
  | Withdrew(string)
  | Answered(string, Event.answer)
  | Exited
  | Forgotten;

/* In [state], reported if that is a change. */
let now = (s, state) =>
  s.state == state
    ? (s, [])
    : (
      {
        ...s,
        state,
      },
      [Event.Agent_state(state)],
    );

let cancelled = pending =>
  List.map(
    ((id, _)) =>
      Event.Answer({
        id,
        answer: None,
      }),
    pending,
  );

let step = (s, happening) =>
  switch (happening) {
  | Began => now(s, Running)
  | Told(ev) => (s, [Event.Agent(ev)])
  | Ended(ev) =>
    let (next, reported) =
      now(
        {
          ...s,
          finished: s.finished + 1,
          pending: [],
        },
        Idle,
      );
    (next, [Event.Agent(ev)] @ cancelled(s.pending) @ reported);
  | Asked({id, tool, input, suggestions, tool_use}) =>
    let (next, reported) =
      now(
        {
          ...s,
          pending: List.remove_assoc(id, s.pending) @ [(id, input)],
        },
        Waiting,
      );
    (
      next,
      [
        Event.Ask({
          id,
          tool,
          input,
          /* A question is answered, not allowed. */
          patterns:
            tool == "AskUserQuestion"
              ? [] : Permissions.suggest(~tool, ~input, ~suggestions),
          tool_use,
        }),
        ...reported,
      ],
    );
  | Withdrew(id) when List.mem_assoc(id, s.pending) => (
      {
        ...s,
        pending: List.remove_assoc(id, s.pending),
      },
      cancelled([(id, `Null)]),
    )
  | Answered(id, answer) when List.mem_assoc(id, s.pending) =>
    let left = {
      ...s,
      pending: List.remove_assoc(id, s.pending),
    };
    let (next, reported) =
      left.pending == [] ? now(left, Running) : (left, []);
    (
      next,
      [
        Event.Answer({
          id,
          answer: Some(answer),
        }),
        ...reported,
      ],
    );
  | Withdrew(_)
  | Answered(_) => (s, [])
  | Exited =>
    let (next, reported) =
      now(
        {
          ...s,
          pending: [],
        },
        Exited,
      );
    (next, cancelled(s.pending) @ reported);
  | Forgotten => (
      {
        ...s,
        state: Idle,
        pending: [],
      },
      [],
    )
  };

let state = s => s.state;
let finished = s => s.finished;
let waiting = (s, id) => List.assoc_opt(id, s.pending);

type t = {
  project: Project.t,
  m: Mutex.t,
  mutable standing,
  /* What the agent last did to the files; guarded by its own lock. */
  cause_m: Mutex.t,
  mutable cause: option(Event.cause),
};

let make = project => {
  project,
  m: Mutex.create(),
  standing: idle,
  cause_m: Mutex.create(),
  cause: None,
};

let locked = (t, f) => {
  Mutex.lock(t.m);
  Fun.protect(~finally=() => Mutex.unlock(t.m), f);
};

/* Call with [t.m] held. */
let take = (t, happening) => {
  let (next, events) = step(t.standing, happening);
  t.standing = next;
  List.iter(Bus.emit, events);
};

let happened = (t, happening) => {
  switch (happening) {
  | Told(ev)
  | Ended(ev) =>
    switch (Protocol.cause(~relative=Project.relative(t.project), ev)) {
    | Some(cause) =>
      Mutex.lock(t.cause_m);
      t.cause = Some(cause);
      Mutex.unlock(t.cause_m);
    | None => ()
    }
  | _ => ()
  };
  locked(t, () => take(t, happening));
};

let answered = (t, ~id, answer) =>
  locked(
    t,
    () => {
      let input = waiting(t.standing, id);
      if (input != None) {
        take(t, Answered(id, answer));
      };
      input;
    },
  );

let standing = t => locked(t, () => t.standing);

let take_cause = t => {
  Mutex.lock(t.cause_m);
  let cause = t.cause;
  t.cause = None;
  Mutex.unlock(t.cause_m);
  Option.value(cause, ~default=Event.By_hand);
};
