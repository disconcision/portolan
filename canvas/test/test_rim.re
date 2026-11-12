/* Where the modules beside the one shown are put (CanvasRim), and how a
   view names them in the record (CanvasGraph's "rim"). */

open CanvasGraph;

let rim = (r_key, r_side) => {
  r_key,
  r_side,
  r_uses: [],
};

/* two nodes of the module shown, 200 apart, and three modules among them */
let placed = [
  ("a", (0., 0.), 10.),
  ("b", (200., 100.), 10.),
  ("Low", (90., 80.), 20.),
  ("High", (110., 20.), 20.),
  ("Out", (50., 50.), 20.),
];
let rims = [rim("Low", Users), rim("High", Users), rim("Out", Used)];
let at = (k, moved) => List.assoc(k, moved);

let users_left_used_right = () => {
  let moved = CanvasRim.place(rims, placed);
  Alcotest.(check(bool))(
    "users are left of everything shown",
    true,
    fst(at("Low", moved)) < (-10.) && fst(at("High", moved)) < (-10.),
  );
  Alcotest.(check(bool))(
    "what it uses is right of it",
    true,
    fst(at("Out", moved)) > 210.,
  );
};

let a_column_keeps_the_layouts_order = () => {
  let moved = CanvasRim.place(rims, placed);
  Alcotest.(check(bool))(
    "High stays above Low",
    true,
    snd(at("High", moved)) < snd(at("Low", moved)),
  );
};

let only_the_rim_moves = () =>
  Alcotest.(check(list(string)))(
    "the moved",
    ["High", "Low", "Out"],
    List.sort(compare, List.map(fst, CanvasRim.place(rims, placed))),
  );

let no_rim_no_move = () => {
  Alcotest.(check(int))(
    "no rim",
    0,
    List.length(CanvasRim.place([], placed)),
  );
  Alcotest.(check(int))(
    "nothing else on the board",
    0,
    List.length(CanvasRim.place(rims, [("Low", (0., 0.), 20.)])),
  );
};

let empty_record = {|"nodes": [], "edges": [], "values": [], "loose_tests": []|};
let read = extra =>
  switch (CanvasGraph.of_string("{" ++ empty_record ++ extra ++ "}")) {
  | Ok(g) => g.rim
  | Error(e) => Alcotest.fail(e)
  };

let a_view_names_the_rim = () =>
  Alcotest.(check(bool))(
    "two modules",
    true,
    read(
      {|, "rim": [{"key": "A", "side": "users", "uses": ["M.f"]}, {"key": "B", "side": "used"}]|},
    )
    == [
         {
           r_key: "A",
           r_side: Users,
           r_uses: ["M.f"],
         },
         {
           r_key: "B",
           r_side: Used,
           r_uses: [],
         },
       ],
  );

let a_record_without_one_has_none = () => {
  Alcotest.(check(int))("absent", 0, List.length(read("")));
  Alcotest.(check(int))(
    "an unknown side is skipped",
    0,
    List.length(read({|, "rim": [{"key": "A", "side": "above"}]|})),
  );
};

let () = {
  let case = (name, f) => Alcotest.test_case(name, `Quick, f);
  Alcotest.run(
    "rim",
    [
      (
        "place",
        [
          case(
            "users go left, what is used goes right",
            users_left_used_right,
          ),
          case(
            "a column keeps the layout's order",
            a_column_keeps_the_layouts_order,
          ),
          case("only the rim's nodes move", only_the_rim_moves),
          case("no rim, or nothing beside it, moves nothing", no_rim_no_move),
        ],
      ),
      (
        "record",
        [
          case("a view names the rim's modules", a_view_names_the_rim),
          case(
            "a record without one has none",
            a_record_without_one_has_none,
          ),
        ],
      ),
    ],
  );
};
