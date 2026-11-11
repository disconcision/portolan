/* Graph events. A graph is the whole project, so sending it whole at
   every change makes the trace grow fast. A `graph` event therefore
   carries either the whole graph, as `graph`, or the change from the
   graph of the event before it, as `delta` (see [Delta]). The whole
   graph is sent first, and again whenever the changes since the last
   whole one add up to as much as a whole graph, so reading the latest
   graph back never costs much more than reading one graph.

   Traces written before this hold only whole graphs and read the same. */

/** [emit(~cause, ~partial, graph)] puts a `graph {graph | delta, cause}`
    event on the bus, with `partial: true` if [partial]. */
let emit: (~cause: Yojson.Safe.t, ~partial: bool, Yojson.Safe.t) => unit;

/** [resume(lines)] takes up from a trace's events, given as JSON text,
    oldest first: later events are changes from the last graph among
    them. Returns that graph, if there is one. */
let resume: list(string) => option(Yojson.Safe.t);

/** [collapse(lines)] is the events with all their graph events replaced
    by one, holding the latest graph whole, where the last of them was:
    what a viewer arriving late needs. */
let collapse: list(string) => list(string);

/** [compact(lines)] is the events with every graph event encoded as
    [emit] would have: for shrinking a trace written with whole graphs. */
let compact: list(string) => list(string);

/** [starting_over(f)] calls [f] with the fields of a `graph` event
    holding the current graph whole ([None] if there is no graph yet),
    for opening a new trace with it. No graph event is sent while [f]
    runs, and the events after it are changes from that graph. */
let starting_over: (option(list((string, Yojson.Safe.t))) => unit) => unit;
