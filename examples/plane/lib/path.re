/* Paths: lists of points. No interface, so everything is exported. */

type t = list(Point.t);

type ends = option(Point.pair);

let start: t = [Point.origin];

let shift = (d: Point.t, path: t): t => List.map(Point.add(d), path);

let ends = (path: t): ends =>
  switch (path) {
  | [] => None
  | [first, ...rest] =>
    Some((first, List.fold_left((_, p) => p, first, rest)))
  };

let test_shift_start = () =>
  assert(shift({x: 1, y: 2}, start) == [{x: 1, y: 2}]);
