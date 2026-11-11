open Portolan_sidecar;
open Testing;

let locations = Alcotest.(list(pair(string, int)));

let found = (output, expected) =>
  Alcotest.check(locations, output, expected, Watcher.locations(output));

let tests = [
  case("a failing inline test is found at its file and line", () =>
    found(
      "File \"lib/scene.ml\", line 17, characters 0-98: total area adds up is false.\n\nFAILED 1 / 2 tests\n",
      [("lib/scene.ml", 17)],
    )
  ),
  case("an error over several lines is found at its first", () =>
    found(
      "File \"lib/extra.ml\", lines 3-5, characters 2-9:\nError: this is wrong",
      [("lib/extra.ml", 3)],
    )
  ),
  case("locations come in order, each once", () =>
    found(
      "File \"b.ml\", line 2, characters 0-1:\nFile \"a.ml\", line 9:\nFile \"b.ml\", line 2, characters 4-5:\nFile \"b.ml\", line 3:\n",
      [("b.ml", 2), ("a.ml", 9), ("b.ml", 3)],
    )
  ),
  case("output that names no file has no locations", () => {
    found("", []);
    found("FAILED 1 / 2 tests\nFile \"unfinished", []);
    found("File \"lib/a.ml\", somewhere", []);
  }),
];
