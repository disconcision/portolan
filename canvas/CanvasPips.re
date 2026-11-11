let limit = 6;

type shown =
  | Each(list(CanvasGraph.test_info))
  | Folded(list((CanvasGraph.test_info, int)));

/* Failing first, so a failure is never behind a count of passes. */
let rank = (t: CanvasGraph.test_info) =>
  switch (t.status) {
  | Some(Fail) => 0
  | Some(Indet) => 1
  | None => 2
  | Some(Pass) => 3
  };

let shown = tests =>
  if (List.length(tests) <= limit) {
    Each(tests);
  } else {
    Folded(
      List.filter_map(
        r =>
          switch (List.filter(t => rank(t) == r, tests)) {
          | [] => None
          | [first, ...rest] => Some((first, List.length(rest)))
          },
        [0, 1, 2, 3],
      ),
    );
  };

let pip = 9.;

let width = tests =>
  switch (shown(tests)) {
  | Each(ts) => pip *. float_of_int(List.length(ts))
  | Folded(groups) =>
    List.fold_left(
      (w, (_, more)) =>
        w
        +. pip
        +. (
          more > 0
            ? 7. +. 5.5 *. float_of_int(String.length(string_of_int(more)))
            : 0.
        ),
      0.,
      groups,
    )
  };
