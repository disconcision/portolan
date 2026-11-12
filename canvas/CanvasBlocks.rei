/* A module opened in place. A snapshot that carries a [base] (the same
   project with every module closed) is laid out from it: the closed
   modules stay where the base puts them, and each open module is laid out
   on its own and set down where its closed node was, growing right and
   down. What lies right of it moves right by what it grew, what lies below
   moves down, and nothing else moves. Each open module pushes independently
   of the others, so opening or closing one moves the rest by that one's
   growth alone. */

/* every node's key, centre and radius */
type placed = list((string, (float, float), float));

/* The open modules: the base's nodes that are regions here. Sorted. */
let roots: CanvasGraph.t => list(string);

/* An open module's own graph: its members, what they are made of, and the
   arrows among them. */
let sub: (CanvasGraph.t, string) => CanvasGraph.t;

/* [compose(~base, ~blocks)]: the centre of every node. [base] is the closed
   project placed; [blocks] is each open module's name with its own graph
   placed alone. A node two modules both hold is placed by the first. */
let compose:
  (~base: placed, ~blocks: list((string, placed))) =>
  list((string, (float, float)));
