open Portolan_sidecar;
open Testing;

let text = "one\ntwo\nthree\nfour";

let slice = (first, last, expected) =>
  Alcotest.(check(string))(
    Printf.sprintf("from %d", first),
    expected,
    Syntax.slice(~first, ~last, text),
  );

let tests = [
  case("a slice is its first to its last line, both included", () => {
    slice(2, Some(3), "two\nthree");
    slice(4, Some(4), "four");
  }),
  case("a slice without a last line runs to the end", () =>
    slice(3, None, "three\nfour")
  ),
  case("bounds outside the text are clamped", () => {
    slice(0, Some(99), text);
    slice(-3, Some(1), "one");
  }),
  case("a slice that ends before it starts is empty", () => {
    slice(3, Some(2), "");
    slice(9, None, "");
  }),
  case("a syntax is named by its extension", () =>
    Alcotest.(check(bool))(
      "names",
      true,
      Syntax.of_string("re") == Some(Syntax.Re)
      && Syntax.of_string("ml") == Some(Syntax.Ml)
      && Syntax.of_string("rei") == None,
    )
  ),
];
