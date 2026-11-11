/* The watcher: files -> build -> graph -> tests.

   Files on disk are the truth. Whenever the project's sources change,
   whoever changed them, the watcher builds the project's annotations into
   Portolan's own build folder, runs the extractor to draw the graph, and
   runs the tests. It reports each step on the bus:

   - `build {ok, output, ms}` after every build;
   - a `graph` event (see [Graphs]) when the graph differs from the last
     one sent. When the build failed the graph is drawn from what still compiled,
     with the rest carried over from the last graph and the definitions
     holding an error marked; the event then also has `partial: true`;
   - `tests {ok, output, failing: [{file, line}]}` after a good build;
   - `extract {ok: false, output}` if the extractor failed on a good build
     that did compile something;
   - `error {where, message}` if the watcher itself went wrong.

   The last graph sent is kept in `.portolan/last-graph.json`. */

/** Starts watching a project, on a thread of its own. [extractor] is the
    path of the `portolan-extract` executable. [cause] is asked, each time
    a new graph is sent, what led to it; its answer becomes the event's
    `cause`. Picks up from the bus's backlog, so a restart does not resend
    a graph the page already has. */
let start:
  (~extractor: string, ~cause: unit => Yojson.Safe.t, Project.t) => unit;

/** Every `File "lib/scene.ml", line 21` in a compiler's or a test runner's
    output, as (file, line), in order and without repeats. An error over
    several lines (`lines 3-5`) gives its first. */
let locations: string => list((string, int));
