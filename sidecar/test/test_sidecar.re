/* The sidecar's tests: one group for each module that decides something.
   Run with `dune test sidecar`. */

let () =
  Alcotest.run(
    "sidecar",
    [
      ("Delta", Test_delta.tests),
      ("Permissions", Test_permissions.tests),
      ("Syntax", Test_syntax.tests),
      ("Watcher", Test_watcher.tests),
    ],
  );
