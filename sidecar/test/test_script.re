open Portolan_sidecar;
open Testing;

let step = text => Script.step_of_json(parse(text));

let reads = (text, expected) =>
  Alcotest.(check(bool))(text, true, step(text) == Some(expected));

let replaced = Alcotest.(option(string));

let tests = [
  case("each kind of step is read", () => {
    reads({|{"say":"Types are in."}|}, Say("Types are in."));
    reads({|{"wait":2500}|}, Wait(2500));
    reads(
      {|{"write":{"file":"lib/dune","content":"(library)\n"}}|},
      Write({
        file: "lib/dune",
        content: "(library)\n",
      }),
    );
    reads(
      {|{"edit":{"file":"lib/scene.ml","old":"1 sc","new":"0 sc"}}|},
      Edit({
        file: "lib/scene.ml",
        old: "1 sc",
        by: "0 sc",
      }),
    );
    reads(
      {|{"ask":{"tool":"Bash","input":{"command":"make"}}}|},
      Ask({
        tool: "Bash",
        input: parse({|{"command":"make"}|}),
      }),
    );
  }),
  case("anything else is not a step, and is skipped", () =>
    List.iter(
      text => Alcotest.(check(bool))(text, true, step(text) == None),
      [
        {|{"shout":"hello"}|},
        {|{"say":3}|},
        {|{"wait":"long"}|},
        {|{"say":"a","wait":1}|},
        {|"say"|},
      ],
    )
  ),
  case("a step with a part missing is an error", () =>
    List.iter(
      text =>
        switch (step(text)) {
        | exception (Yojson.Safe.Util.Type_error(_)) => ()
        | _ => Alcotest.fail(text)
        },
      [
        {|{"write":{"file":"a.ml"}}|},
        {|{"edit":{"file":"a.ml","old":"x"}}|},
        {|{"ask":{"input":{}}}|},
        {|{"write":"a.ml"}|},
      ],
    )
  ),
  case("an edit replaces the first occurrence only", () =>
    Alcotest.check(
      replaced,
      "text",
      Some("let b = a + a"),
      Script.replace(~old="a", ~by="b", "let a = a + a"),
    )
  ),
  case("an edit takes its old text literally, not as a pattern", () =>
    Alcotest.check(
      replaced,
      "text",
      Some("f(x) + .*"),
      Script.replace(~old=".*", ~by="f(x)", ".* + .*"),
    )
  ),
  case("an edit whose old text is not there does nothing", () =>
    Alcotest.check(
      replaced,
      "text",
      None,
      Script.replace(~old="this text is not there", ~by="x", "let a = 1"),
    )
  ),
];
