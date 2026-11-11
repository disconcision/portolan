/** A point with whole coordinates. */
type t = {
  x: int,
  y: int,
};

type pair = (t, t);

/** Where the axes cross. */
let origin: t;

let add: (t, t) => t;
let double: t => t;

module Grid: {let snap: (int, t) => t;};
