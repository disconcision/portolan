/* The event bus. Everything the sidecar reports (builds, graphs, test
   results, the agent's stream) is an event (see [Event]). Events are
   kept in memory for the page's event stream and appended to the
   project's trace file.

   There is one bus in the process: its state is this module's own.

   Events are kept as the lines they are written as, not as [Event.t]:
   the page and the trace file want text, a graph event is long, and the
   backlog read from a trace may hold events of an older version, which
   are passed on as they are. */

/** Adds an event, stamped with the time on the project's clock, which
    carries on from the trace's last event. */
let emit: Event.t => unit;

/** [record(path)] starts appending events to the trace file at [path].
    Events already in that file become the backlog, as if they had just
    been emitted, and the clock continues from the last of them. Returns
    how many events were loaded. */
let record: string => int;

/** A place in the stream of events. */
type cursor;

/** Before the first event. */
let origin: cursor;

/** Blocks until there are events past the cursor, then returns them,
    oldest first, with the cursor past them. The flag is true when they
    are the events of a trace from its start: at the first wait from
    [origin], and after [start_over]. */
let wait: cursor => (list(Event.Line.t), cursor, bool);

/** [start_over(~archive, opening)] moves the trace file to the path
    [archive] and starts a new, empty trace in its place, opening with
    the events [opening]. The events so far are forgotten and the clock
    starts from zero. */
let start_over: (~archive: string, list(Event.t)) => unit;

/** Every event so far, oldest first. */
let all: unit => list(Event.Line.t);

/** The most recent event of a kind, if there is one and this version can
    read it. */
let last: Event.Kind.t => option(Event.t);
