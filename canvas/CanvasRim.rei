/* The rim: where the modules beside the one a board shows are put. Those
   that use it stand in a column to its left, those it uses to its right,
   each column in the order the layout left them. */

/* [place(rim, placed)]: the new centres of the rim's nodes, given every
   node's key, centre and radius. Empty when no rim node is on the board,
   or nothing else is. */
let place:
  (list(CanvasGraph.rim), list((string, (float, float), float))) =>
  list((string, (float, float)));
