open Portolan_sidecar;
open Testing;

let event =
  Alcotest.testable(
    (ppf, e) =>
      Format.pp_print_string(ppf, Yojson.Safe.to_string(Event.to_json(e))),
    (==),
  );

/* Every event, with its JSON as the page and the traces have it. */
let written: list((Event.t, string)) = [
  (Session({restart: true}), {|{"type":"session","restart":true}|}),
  (
    Hello({
      project: "scene",
      agent: "claude",
      mode: Live,
      opam_switch: None,
    }),
    {|{"type":"hello","project":"scene","agent":"claude","mode":"live"}|},
  ),
  (
    Hello({
      project: "scene",
      agent: "none",
      mode: Snapshot,
      opam_switch: None,
    }),
    {|{"type":"hello","project":"scene","agent":"none","mode":"snapshot"}|},
  ),
  (
    Hello({
      project: "scene",
      agent: "script:steps.json",
      mode: Live,
      opam_switch: Some("5.2.0"),
    }),
    {|{"type":"hello","project":"scene","agent":"script:steps.json","mode":"live","switch":"5.2.0"}|},
  ),
  (
    Conversation({earlier: "conversations/2026-11-11T07-12-33Z.jsonl"}),
    {|{"type":"conversation","earlier":"conversations/2026-11-11T07-12-33Z.jsonl"}|},
  ),
  (
    Build({
      ok: false,
      output: "Error: no",
      ms: 141,
    }),
    {|{"type":"build","ok":false,"output":"Error: no","ms":141}|},
  ),
  (
    Graph({
      drawing: Whole(parse({|{"nodes":[],"edges":[]}|})),
      cause: By_hand,
      partial: false,
    }),
    {|{"type":"graph","graph":{"nodes":[],"edges":[]},"cause":{"label":"edit","by":"hand"}}|},
  ),
  (
    Graph({
      drawing: Change(parse({|{"drop":["values"]}|})),
      cause:
        Wrote({
          tool: "Edit",
          file: "lib/scene.ml",
        }),
      partial: true,
    }),
    {|{"type":"graph","delta":{"drop":["values"]},"cause":{"label":"Edit","tool":"Edit","file":"lib/scene.ml","by":"agent"},"partial":true}|},
  ),
  (
    Graph({
      drawing: Change(parse("{}")),
      cause: Ran("rm -rf _build"),
      partial: false,
    }),
    {|{"type":"graph","delta":{},"cause":{"label":"Bash","tool":"Bash","command":"rm -rf _build","by":"agent"}}|},
  ),
  (
    Graph({
      drawing: Change(parse("{}")),
      cause: Tested,
      partial: false,
    }),
    {|{"type":"graph","delta":{},"cause":{"label":"tests"}}|},
  ),
  (
    Graph({
      drawing: Whole(parse("{}")),
      cause: Carried_over,
      partial: false,
    }),
    {|{"type":"graph","graph":{},"cause":{"label":"carried over"}}|},
  ),
  (
    Tests({
      ok: false,
      output: "FAILED 1 / 2 tests\n",
      failing: [("lib/scene.ml", 17)],
    }),
    {|{"type":"tests","ok":false,"output":"FAILED 1 / 2 tests\n","failing":[{"file":"lib/scene.ml","line":17}]}|},
  ),
  (
    Extract_failed("no annotations"),
    {|{"type":"extract","ok":false,"output":"no annotations"}|},
  ),
  (
    Error({
      where: "watcher",
      message: "Not_found",
    }),
    {|{"type":"error","where":"watcher","message":"Not_found"}|},
  ),
  (
    Prompt({
      text: "Add a perimeter.",
      about: None,
    }),
    {|{"type":"prompt","text":"Add a perimeter."}|},
  ),
  (
    Prompt({
      text: "Why twice?",
      about:
        Some({
          name: "Scene.area",
          where: Some(("lib/scene.ml", 8, 11)),
        }),
    }),
    {|{"type":"prompt","text":"Why twice?","about":{"name":"Scene.area","file":"lib/scene.ml","line":8,"end_line":11}}|},
  ),
  (
    Prompt({
      text: "And this?",
      about:
        Some({
          name: "Scene.shape",
          where: None,
        }),
    }),
    {|{"type":"prompt","text":"And this?","about":{"name":"Scene.shape"}}|},
  ),
  (
    Agent(parse({|{"type":"result","subtype":"success","is_error":false}|})),
    {|{"type":"agent","ev":{"type":"result","subtype":"success","is_error":false}}|},
  ),
  (Agent_state(Running), {|{"type":"agent_state","state":"running"}|}),
  (Agent_state(Waiting), {|{"type":"agent_state","state":"waiting"}|}),
  (Agent_state(Idle), {|{"type":"agent_state","state":"idle"}|}),
  (Agent_state(Exited), {|{"type":"agent_state","state":"exited"}|}),
  (
    Agent_raw("this is not json"),
    {|{"type":"agent_raw","line":"this is not json"}|},
  ),
  (
    Ask({
      id: "req_a",
      tool: "Bash",
      input: parse({|{"command":"rm -rf old"}|}),
      patterns: ["Bash(rm -rf old)"],
      tool_use: Some("toolu_b"),
    }),
    {|{"type":"ask","id":"req_a","tool":"Bash","input":{"command":"rm -rf old"},"patterns":["Bash(rm -rf old)"],"tool_use_id":"toolu_b"}|},
  ),
  (
    Ask({
      id: "req_c",
      tool: "Read",
      input: parse({|{"file_path":"/etc/hosts"}|}),
      patterns: [],
      tool_use: None,
    }),
    {|{"type":"ask","id":"req_c","tool":"Read","input":{"file_path":"/etc/hosts"},"patterns":[]}|},
  ),
  (
    Answer({
      id: "req_a",
      answer: Some(Once),
    }),
    {|{"type":"answer","id":"req_a","decision":"once"}|},
  ),
  (
    Answer({
      id: "req_a",
      answer: Some(Always(["Bash(git push:*)", "Bash(python3:*)"])),
    }),
    {|{"type":"answer","id":"req_a","decision":"always","patterns":["Bash(git push:*)","Bash(python3:*)"]}|},
  ),
  (
    Answer({
      id: "req_a",
      answer: Some(Deny),
    }),
    {|{"type":"answer","id":"req_a","decision":"deny"}|},
  ),
  (
    Answer({
      id: "req_q",
      answer: Some(Answers([("Which?", "B"), ("How many?", "One, Two")])),
    }),
    {|{"type":"answer","id":"req_q","decision":"answered","answers":{"Which?":"B","How many?":"One, Two"}}|},
  ),
  (
    Answer({
      id: "req_h",
      answer: None,
    }),
    {|{"type":"answer","id":"req_h","decision":"cancelled"}|},
  ),
];

/* The recorded traces, which the tests' dune file copies beside them. */
let traces = "../../site/runs";

let recorded = () =>
  Sys.readdir(traces)
  |> Array.to_list
  |> List.filter(name => Filename.check_suffix(name, ".jsonl"))
  |> List.concat_map(name =>
       Files.lines(Files.read(Filename.concat(traces, name)))
       |> List.map(Event.Line.of_string)
     );

let line = Event.Line.to_string;

let tests = [
  case("each event is written as the page reads it", () =>
    List.iter(
      ((e, text)) =>
        Alcotest.(check(string))(
          text,
          text,
          Yojson.Safe.to_string(Event.to_json(e)),
        ),
      written,
    )
  ),
  case("an event reads back as it was written", () =>
    List.iter(
      ((e, text)) =>
        Alcotest.(check(option(event)))(
          text,
          Some(e),
          Event.of_json(Event.to_json(e)),
        ),
      written,
    )
  ),
  case("the recorded traces read back as they were written", () => {
    let lines = recorded();
    Alcotest.(check(bool))("there are traces", true, lines != []);
    List.iter(
      l =>
        switch (
          Event.Line.kind(l),
          Event.Line.time(l),
          Event.Line.event(l),
        ) {
        | (Some(_), Some(time), Some(e)) =>
          /* Through the parser, since a published trace may spell a
             character another way than the sidecar wrote it. */
          Alcotest.(check(string))(
            "line",
            Yojson.Safe.to_string(parse(line(l))),
            line(Event.Line.make(~time, e)),
          )
        | (Some(_), _, _) => Alcotest.fail("not read: " ++ line(l))
        /* A kind from before this version, passed on as it is. */
        | (None, _, _) => ()
        },
      lines,
    );
  }),
  case("an event of a kind this version does not know is not read", () => {
    let files = {|{"t":3,"type":"files","files":["lib/scene.ml"]}|};
    Alcotest.(check(option(event)))(
      "event",
      None,
      Event.Line.event(Event.Line.of_string(files)),
    );
    Alcotest.(check(bool))(
      "kind",
      true,
      Event.Line.kind(Event.Line.of_string(files)) == None,
    );
  }),
  case("an event missing a field is not read", () =>
    List.iter(
      text =>
        Alcotest.(check(option(event)))(
          text,
          None,
          Event.of_json(parse(text)),
        ),
      [
        {|{"type":"build","ok":true,"output":""}|},
        {|{"type":"tests","ok":true,"output":"","failing":[{"file":"a.ml"}]}|},
        {|{"type":"agent_state","state":"asleep"}|},
        {|{"type":"graph","cause":{"label":"tests"}}|},
        {|{"type":"answer","id":"a","decision":"perhaps"}|},
        {|["type","session"]|},
      ],
    )
  ),
  case("a line has the time first and the kind second", () => {
    let l =
      Event.Line.make(
        ~time=1424,
        Build({
          ok: true,
          output: "",
          ms: 1136,
        }),
      );
    Alcotest.(check(string))(
      "text",
      {|{"t":1424,"type":"build","ok":true,"output":"","ms":1136}|},
      line(l),
    );
    Alcotest.(check(bool))(
      "kind",
      true,
      Event.Line.kind(l) == Some(Event.Kind.Build),
    );
    Alcotest.(check(option(int)))("time", Some(1424), Event.Line.time(l));
  }),
  case("a line cut short is not whole", () => {
    let whole = text => Event.Line.is_whole(Event.Line.of_string(text));
    Alcotest.(check(bool))("whole", true, whole({|{"t":1,"type":"x"}|}));
    Alcotest.(check(bool))("cut", false, whole({|{"t":1,"type":"agen|}));
    Alcotest.(check(option(int)))(
      "no time",
      None,
      Event.Line.time(Event.Line.of_string({|{"t":1,"type":"agen|})),
    );
  }),
  case("a graph line says off its front whether it is whole", () => {
    let draws = text => Event.Line.draws(Event.Line.of_string(text));
    Alcotest.(check(bool))(
      "whole",
      true,
      draws({|{"t":1,"type":"graph","graph":{},"cause":{}}|}) == Some(`Whole),
    );
    Alcotest.(check(bool))(
      "change",
      true,
      draws({|{"t":1,"type":"graph","delta":{},"cause":{}}|})
      == Some(`Change),
    );
    Alcotest.(check(bool))(
      "another event",
      true,
      draws({|{"t":1,"type":"build","ok":true,"output":"graph","ms":1}|})
      == None,
    );
  }),
  case("a graph line is redrawn with the rest as it was written", () => {
    let old = {|{"t":9,"type":"graph","delta":{"drop":["x"]},"cause":{"label":"from an older version"},"partial":true,"extra":1}|};
    Alcotest.(check(string))(
      "redrawn",
      {|{"t":9,"type":"graph","graph":{"nodes":[]},"cause":{"label":"from an older version"},"partial":true,"extra":1}|},
      line(
        Event.Line.redraw(
          Event.Line.of_string(old),
          Whole(parse({|{"nodes":[]}|})),
        ),
      ),
    );
    Alcotest.(check(bool))(
      "its change is read though its cause is not known",
      true,
      Event.Line.drawing(Event.Line.of_string(old))
      == Some(Change(parse({|{"drop":["x"]}|}))),
    );
  }),
  case("the page's answer reads as the answer event's does", () => {
    let answer = text => Event.answer_of_json(parse(text));
    Alcotest.(check(bool))(
      "once",
      true,
      answer({|{"id":"a","decision":"once"}|}) == Some(Once),
    );
    Alcotest.(check(bool))(
      "always, keeping only the patterns that are text",
      true,
      answer({|{"decision":"always","patterns":["Bash(ls:*)",3,""]}|})
      == Some(Always(["Bash(ls:*)", ""])),
    );
    Alcotest.(check(bool))(
      "always with no patterns",
      true,
      answer({|{"decision":"always"}|}) == Some(Always([])),
    );
    Alcotest.(check(bool))(
      "answers",
      true,
      answer({|{"decision":"answered","answers":{"Which?":"B"}}|})
      == Some(Answers([("Which?", "B")])),
    );
  }),
  case("what is not an answer of the user's is refused", () =>
    List.iter(
      text =>
        Alcotest.(check(bool))(
          text,
          true,
          Event.answer_of_json(parse(text)) == None,
        ),
      [
        {|{"decision":"cancelled"}|},
        {|{"decision":"perhaps"}|},
        {|{"decision":"answered"}|},
        {|{"decision":"answered","answers":{"Which?":2}}|},
        {|{"patterns":[]}|},
        {|"once"|},
      ],
    )
  ),
  case("a prompt is about a name, with its place if all of it is given", () => {
    let about = text => Event.about_of_json(parse(text));
    Alcotest.(check(bool))(
      "placed",
      true,
      about(
        {|{"name":"Scene.area","file":"lib/scene.ml","line":8,"end_line":11}|},
      )
      == Some({
           name: "Scene.area",
           where: Some(("lib/scene.ml", 8, 11)),
         }),
    );
    Alcotest.(check(bool))(
      "half a place is none",
      true,
      about({|{"name":"Scene.area","file":"lib/scene.ml"}|})
      == Some({
           name: "Scene.area",
           where: None,
         }),
    );
    Alcotest.(check(bool))(
      "no name",
      true,
      about({|{"file":"lib/scene.ml"}|}) == None && about("null") == None,
    );
  }),
  case("long output is cut, and says so", () => {
    Alcotest.(check(string))("short", "abc", Event.clip("abc"));
    Alcotest.(check(string))(
      "long",
      "abcd\n…",
      Event.clip(~n=4, "abcdefgh"),
    );
    Alcotest.(check(int))(
      "the usual limit",
      6000 + String.length("\n…"),
      String.length(Event.clip(String.make(9000, 'x'))),
    );
  }),
];
