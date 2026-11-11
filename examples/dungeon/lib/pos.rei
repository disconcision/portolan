/* A cell on the dungeon grid. x grows east, y grows south. */
type t;

/* One of the four compass directions a thing can move in. */
type dir =
  | North
  | South
  | East
  | West;

/* The position at column x, row y. */
let make: (int, int) => t;

/* The column of a position. */
let x: t => int;

/* The row of a position. */
let y: t => int;

/* True when two positions name the same cell. */
let equal: (t, t) => bool;

/* Orders positions by row, then by column, for use in sets and maps. */
let compare: (t, t) => int;

/* The four directions, in the order North, South, East, West. */
let all_dirs: list(dir);

/* The cell one step away in a direction. */
let step: (t, dir) => t;

/* The four cells one step away, in the order of all_dirs. */
let neighbors: t => list(t);

/* The grid distance between two cells: steps along rows plus steps along columns. */
let manhattan: (t, t) => int;

/* A position written as "(x,y)". */
let to_string: t => string;
