open Portolan_sidecar;
open Testing;

let text = "one\ntwo\nthree\nfour";

let slice = (first, last, expected) =>
  Alcotest.(check(string))(
    Printf.sprintf("from %d", first),
    expected,
    Syntax.slice(~first, ~last, text),
  );

let split = (name, ~count, converted, expected) =>
  Alcotest.(check(option(strings)))(
    name,
    expected,
    Syntax.split_types(~count, converted),
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
  case("each converted type comes back on one line, in order", () =>
    split(
      "three",
      ~count=3,
      "type p0 = (int, int);\ntype p1 =\n  | Circle(point, int)\n  | Rect(point, point);\ntype p2 = {\n  name: string,\n  where: option((string, int, int)),\n};\n",
      Some([
        "(int, int)",
        "Circle(point, int) | Rect(point, point)",
        "{name: string, where: option((string, int, int))}",
      ]),
    )
  ),
  case("a function type broken over lines is joined", () =>
    split(
      "one",
      ~count=1,
      "type p0 =\n  (\n    ~first: int,\n    ~last: option(int),\n    string,\n  ) =>\n  string;\n",
      Some(["(~first: int, ~last: option(int), string) => string"]),
    )
  ),
  case("a batch that came back with a type missing is rejected", () => {
    split(
      "two of three",
      ~count=3,
      "type p0 = int;\ntype p1 = string;\n",
      None,
    );
    split("nothing", ~count=1, "", None);
  }),
];
