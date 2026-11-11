/* A window over a tally, in Reason. */

type t = {
  low: int,
  high: int,
};

/* The signature runs over several lines before the body starts. */
module Clamp: {
  let low: (t, int) => int;
  let both: (t, int) => int;
} = {
  let low = (w: t, n: int): int => max(w.low, n);
  let high = (w: t, n: int): int => min(w.high, n);
  let both = (w: t, n: int): int => high(w, low(w, n));
};

let inside = (w: t, tally: Tally.t): Tally.t =>
  List.filter(((_, n)) => Clamp.both(w, n) == n, tally);
