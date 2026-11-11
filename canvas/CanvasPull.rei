/* CanvasPull — draws the members of each module toward one another, after
   a layout has placed every node.

   An experiment (logbook entry 15). A layout does not try to keep a
   module's members near each other, so a module's region comes out as
   long as its arrows. This pass moves each member a share of the way to
   its module's centre, round by round, keeping nodes clear of each other
   and arrows long enough for their labels, and moving a node out of a
   module that has closed around it. It is a tendency and not a box: a
   module whose functions run between other modules' types stays
   stretched.

   One setting per page, like the layout and the edge style. */

type strength =
  | Mild
  | Strong;

/* (id, label), in the order a menu shows them. The first, "none", is the
   default. */
let strengths: list((string, string));

/* The one in use (None for "none"), and its id. */
let chosen: unit => option(strength);
let id: unit => string;

/* Choose by id. False, and nothing changes, for an id not on offer. */
let set: string => bool;

type point = (float, float);

/* Where every node goes: `nodes` is (key, centre, radius) as the layout
   placed them, and the result has a place for each of those keys, moved
   by whole steps of the lattice.

   A node's module is `CanvasContour.home` of it; a nested module's
   members are drawn toward its centre and toward its parent's. A
   satellite moves with the node it is docked to while both are of one
   module; a terminal docked to another module's type is let go, and
   goes home with the rest of its module. Modules side by side move
   apart, each as one piece, until there is room for both. Nodes of
   different modules end far enough apart that the circles a region
   draws around them do not merge; nodes of one module are not pushed
   further apart than the layout had them, unless they overlapped or an
   arrow between them had no room for its label. */
let apply:
  (strength, CanvasGraph.t, list((string, point, float))) =>
  list((string, point));
