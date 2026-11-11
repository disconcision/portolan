open Portolan_sidecar;
open Testing;

let tests = [
  case("the --agent flag names Claude Code or a script file", () => {
    let kind = text => Agent.kind_of_string(text);
    Alcotest.(check(bool))(
      "kinds",
      true,
      kind("claude") == Some(Agent.Claude)
      && kind("script:steps.json") == Some(Script("steps.json"))
      && kind("script:/a b/steps.json") == Some(Script("/a b/steps.json")),
    );
    Alcotest.(check(bool))(
      "not kinds",
      true,
      kind("script:") == None && kind("Claude") == None && kind("") == None,
    );
  }),
];
