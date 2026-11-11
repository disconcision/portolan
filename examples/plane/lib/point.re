/* Points on the plane, in Reason. */

type t = {
  x: int,
  y: int,
};

type pair = (t, t);

let origin: t = {x: 0, y: 0};

let add = (a: t, b: t): t => {
  x: a.x + b.x,
  y: a.y + b.y,
};

/* private: not in the interface */
let factor = 2;

let double = (p: t): t => {
  x: p.x * factor,
  y: p.y * factor,
};

module Grid = {
  /* private: not in the interface */
  let floor = (step: int, n: int): int => n / step * step;

  let snap = (step: int, p: t): t => {
    x: floor(step, p.x),
    y: floor(step, p.y),
  };
};
