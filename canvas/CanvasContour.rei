/* CanvasContour — what a module's region is drawn around.

   An experiment (logbook entry 15). A region is a set of shapes that a
   filter merges into one blob (CanvasView draws it). Hazel's are a circle
   at each member and a band along each of the module's arrows, so a
   region is as long as its arrows are. The other two leave the arrows
   out: circles at the members, joined into one shape by the fewest thin
   links, and that again with its hollows filled in.

   One setting per page, like the layout and the edge style. */

type t =
  | Arrows /* Hazel's: members, and a band along every arrow */
  | Members /* members only, joined by thin links */
  | Smooth; /* members only, the hollows between near members filled */

/* (id, label), in the order a menu shows them. The first is the default. */
let styles: list((string, string));

/* The one in use, and its id. */
let style: unit => t;
let chosen: unit => string;

/* Choose by id. False, and nothing changes, for an id not on offer. */
let set: string => bool;

/* The module a node is drawn with, by key: its own, or, for a built-in
   type that has none because several functions share it, the module that
   every function it is an end of is in. [] for no module. Arrows keeps to
   a node's own module; the other contours, and the pull, go by this. */
let home:
  (list(CanvasGraph.tynode), list(CanvasGraph.edge), string) => list(string);

type point = (float, float);

/* The links that make circles (id, centre, radius) one connected shape:
   a shortest tree over them, measured rim to rim, as pairs of ends. Each
   end is a circle's id and centre. Circles that overlap are already
   joined, and are given no link. */
let links:
  list((string, point, float)) =>
  list(((string, point), (string, point)));

/* Smooth draws every shape this much larger, in board units, and its
   filter takes as much back off the outside: a hollow between members
   about twice this wide fills in, and a member that stands alone keeps
   the circle it has under Members. */
let swell: float;
