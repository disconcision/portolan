/* Graph events. A graph is the whole project, so sending it whole at
   every change makes the trace grow fast. A `graph` event therefore
   carries either the whole graph, as `graph`, or the change from the
   graph of the event before it, as `delta` (see [Delta]). The whole
   graph is sent first, and again whenever the changes since the last
   whole one add up to as much as a whole graph, so reading the latest
   graph back never costs much more than reading one graph.

   Traces written before this hold only whole graphs and read the same.

   The graph last sent is this module's own state: one per process, as
   the bus is. */

/** [emit(~cause, ~partial, graph)] puts a graph event on the bus, with
    the graph whole or as a change, whichever [encode] chooses. */
let emit: (~cause: Event.cause, ~partial: bool, Yojson.Safe.t) => unit;

/** How to send [graph] after [previous]: as a change if that is less
    than half the size of the graph and the changes sent since the last
    whole graph ([since], in bytes) would still add up to less than one
    graph; otherwise whole. Returns the choice with the new count of
    bytes since a whole graph. */
let encode:
  (~previous: option(Yojson.Safe.t), ~since: int, Yojson.Safe.t) =>
  (Event.drawing, int);

/** [resume(lines)] takes up from a trace's events, oldest first: later
    events are changes from the last graph among them. Returns that
    graph, if there is one. */
let resume: list(Event.Line.t) => option(Yojson.Safe.t);

/** [collapse(lines)] is the events with all their graph events replaced
    by one, holding the latest graph whole, where the last of them was:
    what a viewer arriving late needs. */
let collapse: list(Event.Line.t) => list(Event.Line.t);

/** [compact(lines)] is the events with every graph event encoded as
    [emit] would have: for shrinking a trace written with whole graphs. */
let compact: list(Event.Line.t) => list(Event.Line.t);

/** [starting_over(f)] calls [f] with a graph event holding the current
    graph whole ([None] if there is no graph yet), for opening a new
    trace with it. No graph event is sent while [f] runs, and the events
    after it are changes from that graph. */
let starting_over: (option(Event.t) => unit) => unit;
