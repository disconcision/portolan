/* The event bus. Everything the sidecar reports (builds, graphs, test
   results, the agent's stream) is one JSON object per event. Events are
   kept in memory for the page's event stream and appended to the
   project's trace file. */

/** Adds an event. The fields are prefixed with ["t"], milliseconds on the
    project's clock, which carries on from the trace's last event. By
    convention the first field is ["type"]. */
let emit: list((string, Yojson.Safe.t)) => unit;

/** [record(path)] starts appending events to the trace file at [path].
    Events already in that file become the backlog, as if they had just
    been emitted, and the clock continues from the last of them. Returns
    how many events were loaded. */
let record: string => int;

/** Blocks until there are events at or past the given index, then
    returns all of them, as JSON text, oldest first. */
let wait_from: int => list(string);

/** Every event so far, as JSON text, oldest first. */
let all: unit => list(string);

/** The most recent event of the given type, if there is one. */
let last: string => option(Yojson.Safe.t);

/** Cuts a string to at most [n] bytes (6000 unless given), so one long
    output does not swamp an event. */
let clip: (~n: int=?, string) => string;
