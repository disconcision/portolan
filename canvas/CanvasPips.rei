/* How a function's tests are shown on its label: one pip each while they
   are few; past [limit], one pip per status with a count of the rest. */

/** Past this many tests on one function, the pips fold. */
let limit: int;

type shown =
  | /** Few enough to draw one pip each, in the order given. */
    Each(
      list(CanvasGraph.test_info),
    )
  | /** One pip per status, failing first: the first test of that status
        and how many more of it there are. */
    Folded(
      list((CanvasGraph.test_info, int)),
    );

let shown: list(CanvasGraph.test_info) => shown;

/** The width in pixels the pips take on a label, for layouts that leave
    room before the browser has measured it. */
let width: list(CanvasGraph.test_info) => float;
