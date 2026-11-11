open Portolan_sidecar;
open Testing;

let change = (old, now) =>
  switch (Delta.diff(parse(old), parse(now))) {
  | Some(change) => change
  | None => Alcotest.fail("no change between two objects")
  };

/* The change from [old] to [now], applied to [old], is [now]. */
let round_trip = (old, now) =>
  Alcotest.check(
    json,
    old ++ " -> " ++ now,
    parse(now),
    Delta.apply(parse(old), change(old, now)),
  );

let tests = [
  case("nothing changed is an empty change", () =>
    Alcotest.check(
      json,
      "change",
      parse("{}"),
      change({|{"a":1,"b":[1,2]}|}, {|{"a":1,"b":[1,2]}|}),
    )
  ),
  case("a new value is set and a missing field is dropped", () =>
    Alcotest.check(
      json,
      "change",
      parse({|{"set":{"a":2,"c":3},"drop":["b"]}|}),
      change({|{"a":1,"b":2}|}, {|{"a":2,"c":3}|}),
    )
  ),
  case("a list that grew is the old run and the items added", () =>
    Alcotest.check(
      json,
      "change",
      parse({|{"lists":{"b":[[0,3],{"add":[4]}]}}|}),
      change({|{"a":1,"b":[1,2,3]}|}, {|{"a":1,"b":[1,2,3,4]}|}),
    )
  ),
  case("an item taken out of a list leaves two runs", () =>
    Alcotest.check(
      json,
      "change",
      parse({|{"lists":{"b":[[0,1],[2,2]]}}|}),
      change({|{"b":["x","y","z","w"]}|}, {|{"b":["x","z","w"]}|}),
    )
  ),
  case("a change applied to the old object gives the new one", () => {
    round_trip({|{"a":1,"b":[1,2,3]}|}, {|{"a":1,"b":[3,1,9,2]}|});
    round_trip(
      {|{"a":[1,1,2],"b":{"c":1}}|},
      {|{"b":{"c":2},"a":[2,1,1,1]}|},
    );
    round_trip({|{"a":[1,2]}|}, {|{"a":"now text","z":[]}|});
    round_trip(
      {|{"a":[{"k":1},{"k":2}]}|},
      {|{"a":[{"k":2},{"k":3},{"k":1}]}|},
    );
  }),
  case("fields keep their order and new ones go last", () =>
    Alcotest.(check(string))(
      "applied",
      {|{"a":1,"b":5,"c":3}|},
      Yojson.Safe.to_string(
        Delta.apply(
          parse({|{"a":1,"b":2}|}),
          change({|{"a":1,"b":2}|}, {|{"c":3,"b":5,"a":1}|}),
        ),
      ),
    )
  ),
  case("only two objects have a change", () =>
    Alcotest.(check(bool))(
      "none",
      true,
      Delta.diff(parse("[1]"), parse("{}")) == None,
    )
  ),
];
