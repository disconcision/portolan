/* The sidecar's tests: one group for each module that decides something.
   Run with `dune test sidecar`. */

let () =
  Alcotest.run(
    "sidecar",
    [
      ("Event", Test_event.tests),
      ("Delta", Test_delta.tests),
      ("Graphs", Test_graphs.tests),
      ("Permissions", Test_permissions.tests),
      ("Protocol", Test_protocol.tests),
      ("Turn", Test_turn.tests),
      ("Claude", Test_claude.tests),
      ("Script", Test_script.tests),
      ("Agent", Test_agent.tests),
      ("Syntax", Test_syntax.tests),
      ("Watcher", Test_watcher.tests),
      ("Http", Test_http.tests),
    ],
  );
