/* A module opened in place (CanvasBlocks): what moves, and by how much. */

/* a closed project on a 100 lattice: M in the middle, one node on each side */
let base = [
  ("M", (100., 100.), 20.),
  ("Left", (0., 100.), 20.),
  ("Above", (100., 0.), 20.),
  ("Right", (200., 100.), 20.),
  ("Below", (100., 200.), 20.),
  ("Far", (200., 200.), 20.),
];
/* M's own members, placed alone: a box 300 wide, 100 high */
let own = [("M.a", (10., 10.), 10.), ("M.b", (290., 90.), 10.)];
let at = (k, placed) => List.assoc(k, placed);
let pair = Alcotest.(pair(float(0.01), float(0.01)));
let opened = CanvasBlocks.compose(~base, ~blocks=[("M", own)]);

let left_and_above_stay = () => {
  Alcotest.check(pair, "left", (0., 100.), at("Left", opened));
  Alcotest.check(pair, "above", (100., 0.), at("Above", opened));
};

let right_and_below_make_room = () => {
  let (rx, ry) = at("Right", opened)
  and (bx, by) = at("Below", opened)
  and (fx, fy) = at("Far", opened);
  Alcotest.(check(bool))(
    "right moved right only",
    true,
    rx > 200. && ry == 100.,
  );
  Alcotest.(check(bool))(
    "below moved down only",
    true,
    bx == 100. && by > 200.,
  );
  Alcotest.(check(bool))(
    "the far corner moved both ways, by the same amounts",
    true,
    fx == rx && fy == by,
  );
};

let members_sit_where_the_node_was = () => {
  let (ax, ay) = at("M.a", opened)
  and (bx, by) = at("M.b", opened)
  and (rx, _) = at("Right", opened)
  and (_, ly) = at("Below", opened);
  Alcotest.(check(bool))(
    "the box starts at the closed node's corner",
    true,
    ax > 80. && ay > 80. && ax < 160. && ay < 160.,
  );
  Alcotest.(check(bool))(
    "and ends short of what it pushed",
    true,
    bx +. 10. < rx -. 20. && by +. 10. < ly -. 20.,
  );
  Alcotest.(check(bool))(
    "the closed node itself is gone",
    false,
    List.mem_assoc("M", opened),
  );
};

let each_module_pushes_on_its_own = () => {
  let own_r = [("Right.a", (0., 0.), 10.), ("Right.b", (200., 0.), 10.)];
  let both =
    CanvasBlocks.compose(~base, ~blocks=[("M", own), ("Right", own_r)]);
  let only_right = CanvasBlocks.compose(~base, ~blocks=[("Right", own_r)]);
  let moved = (k, a, b) => (
    fst(at(k, b)) -. fst(at(k, a)),
    snd(at(k, b)) -. snd(at(k, a)),
  );
  /* opening Right with M already open moves a node as opening Right alone does */
  List.iter(
    k =>
      Alcotest.check(
        pair,
        k,
        moved(k, base |> List.map(((k, p, _)) => (k, p)), only_right),
        moved(k, opened, both),
      ),
    ["Left", "Above", "Below", "Far"],
  );
};

let nothing_open_nothing_moves = () =>
  Alcotest.(check(bool))(
    "the base as it was",
    true,
    CanvasBlocks.compose(~base, ~blocks=[])
    == List.map(((k, p, _)) => (k, p), base),
  );

let () = {
  let case = (name, f) => Alcotest.test_case(name, `Quick, f);
  Alcotest.run(
    "blocks",
    [
      (
        "compose",
        [
          case("what is left of it or above it stays", left_and_above_stay),
          case(
            "what is right of it or below it makes room",
            right_and_below_make_room,
          ),
          case(
            "its members sit where its node was",
            members_sit_where_the_node_was,
          ),
          case(
            "each open module pushes on its own",
            each_module_pushes_on_its_own,
          ),
          case(
            "with nothing open, nothing moves",
            nothing_open_nothing_moves,
          ),
        ],
      ),
    ],
  );
};
