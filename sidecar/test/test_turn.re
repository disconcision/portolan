open Portolan_sidecar;
open Testing;

/* Where things stand after these happenings, and what the last reported. */
let after = happenings =>
  List.fold_left(
    ((standing, _), happening) => Turn.step(standing, happening),
    (Turn.idle, []),
    happenings,
  );

let reported = (name, events, expected) =>
  Alcotest.check(
    strings,
    name,
    expected,
    List.map(e => Yojson.Safe.to_string(Event.to_json(e)), events),
  );

let state = (name, standing, expected) =>
  Alcotest.(check(bool))(name, true, Turn.state(standing) == expected);

let make = {|{"command":"make"}|};

let asks = (id, tool, input): Turn.happening =>
  Asked({
    id,
    tool,
    input: parse(input),
    suggestions: `Null,
    tool_use: Some("toolu_" ++ id),
  });

let result = {|{"type":"result","subtype":"success","is_error":false}|};

let tests = [
  case("a turn that begins is reported running, once", () => {
    let (standing, events) = after([Began]);
    reported(
      "first",
      events,
      [{|{"type":"agent_state","state":"running"}|}],
    );
    reported("again", snd(Turn.step(standing, Began)), []);
  }),
  case("the agent's stream is passed on as it is", () => {
    let (standing, events) = after([Began, Told(parse({|{"type":"x"}|}))]);
    reported("event", events, [{|{"type":"agent","ev":{"type":"x"}}|}]);
    state("still running", standing, Running);
  }),
  case("a request makes the agent wait, and offers patterns to allow", () => {
    let (standing, events) = after([Began, asks("a", "Bash", make)]);
    reported(
      "events",
      events,
      [
        {|{"type":"ask","id":"a","tool":"Bash","input":{"command":"make"},"patterns":["Bash(make:*)"],"tool_use_id":"toolu_a"}|},
        {|{"type":"agent_state","state":"waiting"}|},
      ],
    );
    Alcotest.(check(option(json)))(
      "the tool's input is kept for the answer",
      Some(parse(make)),
      Turn.waiting(standing, "a"),
    );
  }),
  case("a question offers nothing to allow", () =>
    reported(
      "events",
      snd(
        after([Began, asks("q", "AskUserQuestion", {|{"questions":[]}|})]),
      ),
      [
        {|{"type":"ask","id":"q","tool":"AskUserQuestion","input":{"questions":[]},"patterns":[],"tool_use_id":"toolu_q"}|},
        {|{"type":"agent_state","state":"waiting"}|},
      ],
    )
  ),
  case("the last answer puts the agent back to work", () => {
    let (standing, _) =
      after([Began, asks("a", "Bash", make), asks("b", "Bash", make)]);
    let (standing, events) = Turn.step(standing, Answered("a", Once));
    reported(
      "one of two",
      events,
      [{|{"type":"answer","id":"a","decision":"once"}|}],
    );
    state("still waiting", standing, Waiting);
    let (standing, events) = Turn.step(standing, Answered("b", Deny));
    reported(
      "the other",
      events,
      [
        {|{"type":"answer","id":"b","decision":"deny"}|},
        {|{"type":"agent_state","state":"running"}|},
      ],
    );
    state("running", standing, Running);
  }),
  case("an answer to a request that is not waiting changes nothing", () => {
    let (standing, _) =
      after([Began, asks("a", "Bash", make), Answered("a", Once)]);
    Alcotest.(check(bool))(
      "answered twice",
      true,
      Turn.step(standing, Answered("a", Deny)) == (standing, []),
    );
    Alcotest.(check(bool))(
      "never asked",
      true,
      Turn.step(standing, Answered("z", Once)) == (standing, [])
      && Turn.step(standing, Withdrew("z")) == (standing, []),
    );
  }),
  case("the end of a turn cancels what still waits, oldest first", () => {
    let (standing, events) =
      after([
        Began,
        asks("a", "Bash", make),
        asks("b", "Bash", make),
        Ended(parse(result)),
      ]);
    reported(
      "events",
      events,
      [
        {|{"type":"agent","ev":{"type":"result","subtype":"success","is_error":false}}|},
        {|{"type":"answer","id":"a","decision":"cancelled"}|},
        {|{"type":"answer","id":"b","decision":"cancelled"}|},
        {|{"type":"agent_state","state":"idle"}|},
      ],
    );
    Alcotest.(check(int))("one turn ended", 1, Turn.finished(standing));
    Alcotest.(check(option(json)))(
      "nothing waits",
      None,
      Turn.waiting(standing, "a"),
    );
  }),
  case(
    "a withdrawn request is cancelled; the agent is still shown waiting", () => {
    let (standing, events) =
      after([Began, asks("x", "Bash", make), Withdrew("x")]);
    reported(
      "events",
      events,
      [{|{"type":"answer","id":"x","decision":"cancelled"}|}],
    );
    state("waiting", standing, Waiting);
  }),
  case("a prompt while a request waits leaves the request to be answered", () => {
    let (standing, events) = after([Began, asks("a", "Bash", make), Began]);
    reported(
      "events",
      events,
      [{|{"type":"agent_state","state":"running"}|}],
    );
    reported(
      "the answer",
      snd(Turn.step(standing, Answered("a", Once))),
      [{|{"type":"answer","id":"a","decision":"once"}|}],
    );
  }),
  case("when the process goes, what waits is cancelled", () => {
    let (standing, events) =
      after([Began, asks("a", "Bash", make), Exited]);
    reported(
      "events",
      events,
      [
        {|{"type":"answer","id":"a","decision":"cancelled"}|},
        {|{"type":"agent_state","state":"exited"}|},
      ],
    );
    reported(
      "the next prompt",
      snd(Turn.step(standing, Began)),
      [{|{"type":"agent_state","state":"running"}|}],
    );
  }),
  case("forgetting says nothing, and nothing waits after it", () => {
    let (standing, events) =
      after([
        Began,
        Ended(parse(result)),
        Began,
        asks("a", "Bash", make),
        Forgotten,
      ]);
    reported("events", events, []);
    state("idle", standing, Idle);
    Alcotest.(check(option(json)))(
      "nothing waits",
      None,
      Turn.waiting(standing, "a"),
    );
    Alcotest.(check(int))(
      "turns still counted",
      1,
      Turn.finished(standing),
    );
  }),
];
