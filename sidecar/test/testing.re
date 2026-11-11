/* What the tests share. */

let case = (name, f) => Alcotest.test_case(name, `Quick, f);

let parse = Yojson.Safe.from_string;

let json =
  Alcotest.testable(
    (ppf, j) => Format.pp_print_string(ppf, Yojson.Safe.to_string(j)),
    Yojson.Safe.equal,
  );

let strings = Alcotest.(list(string));
