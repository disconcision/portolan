/* How a function's tests fold on its label (CanvasPips). */

open CanvasGraph;

let test = (status, n): test_info => {
  t_id: "t" ++ string_of_int(n),
  status,
  t_body: "",
  t_refs: [],
};

let passing = n => List.init(n, test(Some(TestStatus.Pass)));

let few_are_drawn_one_by_one = () =>
  Alcotest.(check(bool))(
    "six stay six",
    true,
    CanvasPips.shown(passing(6)) == Each(passing(6)),
  );

let many_fold_to_one_and_a_count = () =>
  Alcotest.(check(bool))(
    "thirty-one passing",
    true,
    CanvasPips.shown(passing(31)) == Folded([(List.hd(passing(31)), 30)]),
  );

let a_failure_is_never_behind_a_count_of_passes = () => {
  let fail = test(Some(TestStatus.Fail), 99);
  Alcotest.(check(bool))(
    "the failing one leads, alone",
    true,
    CanvasPips.shown(passing(10) @ [fail])
    == Folded([(fail, 0), (List.hd(passing(10)), 9)]),
  );
};

let folded_pips_are_narrower = () =>
  Alcotest.(check(bool))(
    "31 folded take less room than 7 drawn out would",
    true,
    CanvasPips.width(passing(31)) < 7. *. 9.,
  );

let () = {
  let case = (name, f) => Alcotest.test_case(name, `Quick, f);
  Alcotest.run(
    "canvas pips",
    [
      (
        "Pips",
        [
          case("few tests are drawn one by one", few_are_drawn_one_by_one),
          case(
            "many tests fold to one pip and a count",
            many_fold_to_one_and_a_count,
          ),
          case(
            "a failure is never behind a count of passes",
            a_failure_is_never_behind_a_count_of_passes,
          ),
          case("folded pips are narrower", folded_pips_are_narrower),
        ],
      ),
    ],
  );
};
