type t = {
  x: int,
  y: int,
};

type dir =
  | North
  | South
  | East
  | West;

let make = (x, y) => {
  x,
  y,
};

let x = p => p.x;

let y = p => p.y;

let equal = (a, b) => a.x == b.x && a.y == b.y;

let compare = (a, b) =>
  switch (Stdlib.compare(a.y, b.y)) {
  | 0 => Stdlib.compare(a.x, b.x)
  | c => c
  };

let all_dirs = [North, South, East, West];

let step = (p, d) =>
  switch (d) {
  | North => {
      ...p,
      y: p.y - 1,
    }
  | South => {
      ...p,
      y: p.y + 1,
    }
  | East => {
      ...p,
      x: p.x + 1,
    }
  | West => {
      ...p,
      x: p.x - 1,
    }
  };

let neighbors = p => List.map(step(p), all_dirs);

let manhattan = (a, b) => abs(a.x - b.x) + abs(a.y - b.y);

let to_string = p => Printf.sprintf("(%d,%d)", p.x, p.y);

let%test "neighbors are the four cells at distance one" = {
  let p = make(2, 3);
  let ns = neighbors(p);
  List.length(ns) == 4 && List.for_all(n => manhattan(p, n) == 1, ns);
};

let%test "manhattan distance is symmetric" =
  manhattan(make(0, 0), make(3, 4)) == 7
  && manhattan(make(3, 4), make(0, 0)) == 7;

let%test "stepping north then south returns home" =
  equal(step(step(make(1, 1), North), South), make(1, 1));

let%test "compare orders by row then column" =
  compare(make(5, 0), make(0, 1)) < 0
  && compare(make(1, 2), make(0, 2)) > 0;
