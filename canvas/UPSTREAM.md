# The canvas and its upstream

This directory is Hazel's Constellation canvas running outside Hazel: a
js_of_ocaml bundle that draws graph snapshots (the graph record of
`graph/graph.re`, sent as JSON) and
animates the change from one to the next.

- **Upstream:** [hazelgrove/hazel](https://github.com/hazelgrove/hazel), branch
  `agent-canvas` (draft PR #2505), commit `f2c5a38dff`.
- **Spec:** Hazel's canvas principles (A1–F: one score per beat, the actor is
  the only cause, nothing jumps, stability of place, ...), which the copied
  files cite in their comments. Nothing here changes those rules; the code that
  carries them is copied, not rewritten.
- The two code bases are developed in tandem, so the rule is: **a small diff
  beats tidiness.** Copied files are not reformatted, renamed or trimmed.

## Build and run

```sh
eval $(opam env --switch=editor-output --set-switch)   # OCaml 5.2, js_of_ocaml 5.9.1, virtual_dom v0.16, reason 3.15, yojson
dune build ./canvas                                    # -> _build/default/canvas/portolan_canvas_js.bc.js (dev build, about 17 MB)
# dune build --profile release ./canvas                # same path, optimised: 786 KB (253 KB gzipped)
dune test canvas/test                                  # what CanvasGraph makes of a graph record: native, no browser
python3 -m http.server 8843 --bind 127.0.0.1           # at the repository root
# open http://127.0.0.1:8843/canvas/demo.html          (add ?autoplay to start at once)
```

Regenerate the stylesheet, and check the copies against a Hazel checkout:

```sh
python3 canvas/tools/make_css.py /path/to/hazel-worktree     # rewrites canvas/canvas.css
sh canvas/tools/diff_upstream.sh /path/to/hazel-worktree     # upstream/ must be identical; edited/ prints its diff
```

## The API

`window.PortolanCanvas`, defined by `portolan_canvas_js.re`:

| Call | What it does |
|---|---|
| `mount(element)` | Draws the canvas into `element` and fills it. One canvas per page (the copied code finds its elements by id). |
| `push(snapshot, cause)` | A new snapshot and what caused it: one beat. `snapshot` is Graph JSON (string or object). `cause` is `{label, site?, ok?}` (string or object; `err: true` or `status: "err"` also mean not ok). `site` is a definition's qualified name: where the avatar goes. |
| `show(snapshot)` | A snapshot with no story (an existing code base): shown at once and framed whole. |
| `reset()` | A clean board. |
| `setFollow(bool)` | The camera follows the score and the avatar (default on). |
| `setPace(bool)` | Off: snapshots are shown as they arrive, no beats, no scores. |
| `setBusy(bool)` | The agent is working: the avatar is embodied and "thinks" between edits. |
| `setThought(text)` | Streamed reasoning, shown in the thought cloud while busy. |
| `fit()` | Frame the whole graph. |
| `select(name, isType)` | Selects a function or value (or, with `isType`, a type) by its qualified name, as a click would. For a host's search. |
| `onSelect(fn(name, kind, at))` | A click on a function, type, module, value or test pip. `at` is `{file, path, line, end_line}` or `null`. |
| `pending()` | Beats still queued. |
| `log()` | The canvas journal (beats, scores, `SCORE:` / `JUMP:` / `ORPHAN:` checks) as text. |
| `layouts()`, `edgeStyles()` | The layouts and edge styles on offer, each `[{id, label}, ...]`; the first is the default. |
| `layout()`, `edges()` | The ids in use. |
| `setLayout(id)`, `setEdges(id)` | Choose one. The graph on the board is laid out again, glides to its new places and is framed. Returns false for an id not on offer. Positions set by dragging are dropped. |
| `layoutStats()` | `{calls, ms, hits, phases}`: layouts computed, their total time, memo hits, time by phase. For `tools/layout-cost.mjs`. |

Upstream's console testers (`__canvasBurst`, `__canvasStage`, `__cameraState`,
...) are still installed.

## Files

`upstream/` is byte-for-byte; `edited/` is upstream plus edits marked
`PORTOLAN`; `shim/` stands in for Hazel modules the copies mention; the rest
is new.

`CanvasGraph.re` and `shim/Haz3lcore.re` are a library of their own
(`portolan_canvas_graph`), since nothing of the browser is in them: the tests
in `test/` build and run them natively.

| File here | Upstream path | Status | Why |
|---|---|---|---|
| `edited/GraphLayout.re`, `.rei` | `src/util/` | edited (one block, about 75 lines; the `.rei` is unchanged) | The cross-column vertical relief visits only the pairs that can meet. See Layout cost. Worth taking upstream. |
| `upstream/AgentPulse.re` | `src/util/` | verbatim | |
| `upstream/PerfTimer.re` | `src/util/` | verbatim | |
| `upstream/OptUtil.re` | `src/util/` | verbatim | |
| `upstream/Animation.re` | `src/web/` | verbatim | |
| `upstream/CanvasScore.re` | `src/web/app/canvas/` | verbatim | |
| `upstream/CanvasLayoutExperiments.re` | `src/web/app/canvas/` | verbatim | The layout lab: its placements and its "circuit" router. Reachable through `setLayout` / `setEdges` (see Layouts and edge styles). |
| `upstream/CanvasZoom.re` | `src/web/app/canvas/` | verbatim | |
| `upstream/CanvasEnact.re` | `src/web/app/canvas/` | verbatim | |
| `upstream/CanvasAvatar.re` | `src/web/app/canvas/` | verbatim | |
| `upstream/CanvasBubble.re` | `src/web/app/canvas/` | verbatim | |
| `upstream/CanvasCamera.re` | `src/web/app/canvas/` | verbatim | |
| `edited/CanvasRipple.re` | `src/web/app/canvas/` | edited (the dot lattice, about 15 lines) | A quieter lattice: a smaller dot, drawn fainter; below 1:1 every dot fades further and every other one drops out (the level-of-detail band moves from 14–28 px of screen pitch toward 28–56). At 1:1 and closer it is still the lattice node centres snap to. The dot's size is baked into a sprite, so this could not be a stylesheet change. |
| `upstream/CanvasJelly.re` | `src/web/app/canvas/` | verbatim | |
| `upstream/CanvasLog.re` | `src/web/app/canvas/` | verbatim | |
| `edited/CanvasView.re` | `src/web/app/canvas/` | edited (the hull palette, 8 lines) | The six hull colours read `var(--pc-hull-N, <upstream colour>)` so a host can theme them. Worth taking upstream. |
| `edited/CanvasLayout.re` | `src/web/app/canvas/` | edited (hooks for `CanvasOptions`; `layout_impl` cut in two; the memo; table-based copies of two passes) | See Layouts and edge styles, and Layout cost. Every edit is marked `PORTOLAN`; upstream lines are not re-indented, and the two passes that were replaced are still there, unused, above their replacements. |
| `edited/CanvasBuffer.re` | `src/web/app/canvas/` | edited (about 25 lines) | The beat's payload is a graph snapshot instead of Hazel's editor model: `type model = CanvasGraph.t`; `same_program` is graph equality; no "statics pending" state. Dwell rules, queue, hold, staging: untouched. |
| `shim/Util.re` | (`util` library) | shim | Re-exports `JsUtil`, `WebUtil`, `OptUtil`, `PerfTimer`, `GraphLayout`, `AgentPulse`, `Key` under `Util.`, as the copies spell them. |
| `shim/JsUtil.re` | `src/util/JsUtil.re` | shim | 4 of its functions, upstream's bodies (`cached_client_height` without the cache). |
| `shim/WebUtil.re` | `src/util/WebUtil.re` | shim | `clss`, `Node`, `Attr`. |
| `shim/Key.re` | `src/util/Key.re` | shim | `get_key`. |
| `shim/Haz3lcore.re` | (`haz3lcore`) | shim | `Id.t = string`: `CanvasView` names `Haz3lcore.Id.t` in five signatures. |
| `shim/CanvasTrajectory.re` | `src/web/app/canvas/CanvasTrajectory.re` | shim | Two hooks (`on_change`, `busy`) the copies call. |
| `CanvasGraph.re` | `src/web/app/canvas/CanvasGraph.re` | rewritten | Upstream's record types with the same field names (so the copies read them unchanged), ids as strings, terms as source text, plus `handles`. Extraction (lines 160–1606 upstream) is gone; `of_graph` makes the picture from Portolan's graph record (`Portolan_graph.Graph.t`) instead. |
| `CanvasDirector.re` | `src/web/app/canvas/CanvasSidebar.re` | lifted + new | See below. |
| `CanvasOptions.re`, `.rei` | — | new | The layouts and edge styles on offer, and the ones written for Portolan. |
| `portolan_canvas_js.re` | — | new | The global API. |
| `canvas.css` | `src/web/www/style/canvas.css`, `animations.css` | generated | By `tools/make_css.py`: variables renamed `--pc-*`, every literal colour turned into a palette variable, Hazel's values as zero-specificity defaults, `tools/portolan.css` appended. |

### `CanvasDirector.re`

It replaces `CanvasSidebar` + `CanvasPresentation` for the canvas itself. About
nine tenths of it is lifted from `CanvasSidebar.re`, in upstream's order and
wording, with `globals` / `editor` / `settings` swapped for module state:

| Part | From `CanvasSidebar.re` (line numbers at the upstream commit) |
|---|---|
| zoom listener, fit, scroll anchor | 92–324 |
| frame cache, `leaving`, `staged`, looks, snapshots | 326–429 |
| `collapse_filter` | 431–540 |
| `graph_delta` (beat weight) | 641–659 |
| `LayoutDom` (drag follow) | 694–851 |
| `view`: frame derivation | 1148–1171, 1227–1517 |
| `view`: node drag | 1831–2080 |
| `view`: fit | 2595–2632 |
| `view`: avatar, camera follow, bubbles | 3186–3407 |
| `view`: **diff → score → schedules → play** | 4400–5065, 5105–5162 |
| `view`: dependency fan, hull toggle, ghosts and staged looks | 5178–5292, 5369–5532 |

New in it: the render loop (`observe` → `view` → vdom patch → `Animation.go`,
what Hazel's app loop and `Main` do), `push` / `show` / `reset`, selection that
calls the host, `locate` (the avatar's site by definition name rather than by
syntax id), and `avatar_target` (from the last cause rather than the last tool
result).

Deliberate differences from upstream behaviour, each marked `PORTOLAN` in the
code:

1. An **empty board is anchored as if pane-wide**, so the first content lands
   centred instead of off to the right.
2. **`show`** frames the whole graph but never magnifies past 1:1.
3. **`reset`** forgives the dwell owed by the beat that was showing.
4. A pane **resize re-renders** (upstream's app loop does this by itself).
5. Pins, persisted frames and card sizes do not exist; drag offsets live in
   memory until `reset`.

## Layouts and edge styles

Two settings, switchable while a graph is on the board (`setLayout`,
`setEdges`; the page has a select for each and remembers the choice). They
are the lab's own two settings (`CanvasLayoutExperiments.mode` and `.wires`),
so the copied code reads them as it does in Hazel. `CanvasOptions` lists what
is on offer and holds what Portolan added.

| Layout | From | What it does |
|---|---|---|
| `current` (shown as "layered") | Hazel's default | Types in left-to-right layers by dependency and function flow; terminals docked by ring search; stretched to fill the pane. |
| `compact` | Portolan | The same layers, not stretched. An unnamed type used in one place (a record's field type, a lone result) docks beside that place instead of taking a row in a layer. Separate pieces are packed in source order into rows shaped like the pane. |
| `ledger` | Portolan | Each shared type heads a lane: what makes it in rows on its left, what is made from it in rows on its right, in source order. An unnamed type used in one place is a row of that place. Lanes fill columns, a type after the types it is made from. |
| `clusters` | Portolan | The ledger's groups with the rows bent into arcs around the type. |
| `flow`, `dependencies` | lab | The default with only function flow, or only type dependency, deciding the layers. |
| `packed`, `grid`, `stable`, `relaxed` | lab | The default's groups re-placed: shelf-packed, on a grid, grown around what was already placed, or relaxed with soft anchors. |

| Edges | From | What it does |
|---|---|---|
| `curves` | Hazel's default | Horizontal-tangent cubics. |
| `metro` | Portolan | Level runs joined by short diagonals with rounded bends. Edges that share a side of a node leave it side by side and turn in an order that keeps them from crossing each other. The label rides the longest level run. No routing around nodes. |
| `straight` | Portolan | Chords from rim to rim; edges between the same two nodes bow apart. |
| `circuit` | lab | The lab's orthogonal router: avoids nodes, no shared runs. |

The lab's four "research engine" layouts (`elk`, `elk-down`, `cola`,
`cola-live`) are not offered: they need Hazel's worker bundle of elkjs and
WebCoLa (built there with esbuild from `node_modules`), which is not in this
repository. Without it the lab silently falls back to `stable`.

The principles (nothing jumps, stability of place) are kept by the default
only. A switch of layout or edge style is one slow glide. `ledger` and
`clusters` re-derive every place from the graph, in source order, so a new
definition moves its neighbours down a row; `compact` repacks when a piece
grows. None of them has been watched through a long agent run.

## Layout cost

Measured with `tools/layout-cost.mjs` (headless Chrome, the optimised
bundle, a 2000×1200 window) on Portolan's own modules, default layout.
"Drawn" counts every node on the board, terminals included.

| Graph | Drawn | Picking it: layouts, time in layout | One change (remove a function, put it back) |
|---|---|---|---|
| `Watcher` | 48 | 7, 36 ms → 4, 5 ms | 3, 20 ms → 1, 3 ms |
| `Graph` | 83 | 7, 147 ms → 4, 15 ms | 3, 111 ms → 1, 9 ms |
| `CanvasEnact` | 170 | 7, 392 ms → 4, 27 ms | 4, 389 ms → 1, 16 ms |
| `CanvasDirector` | 209 | 7, 522 ms → 4, 28 ms | 3, 394 ms → 1, 16 ms |
| the whole project | about 1850 | 7, 183 s → 4, 2.4 s | not measured before → 2, 2.5 s |

Why it was re-run: a render asks for the same graph in up to four frames
(does the frozen frame still fit; the unstretched layout that the stretch
is derived from; the stretched one; the one drawn), and each was a full
placement. Why each was slow: three passes searched lists by key inside
loops over chords and nodes.

What changed, all giving the same positions as before (node positions and
arrow paths compared against the old bundle on six modules):

1. `layout_impl` is cut where its framing step starts; the memo keeps
   placements by everything but the frame. The default layout now solves
   once per distinct stretch (up to three times for a new graph, counting
   the empty board, where it was seven); a layout that does not stretch
   solves once.
2. `GraphLayout`'s vertical relief: the pairs whose halos overlap in x are
   found once (x does not change there) and the twelve rounds run over
   those on arrays.
3. The declutter pass looked a node's radius up by searching the node list,
   once per chord per node per round: now a table. That pass and the
   docked-chord pass have array and table versions.
4. The default's packing of separate pieces (`Lab.place("components")`) is
   `CanvasOptions.components`: the lab's arithmetic, with tables for roots,
   members and groups.

Not done: the whole project still blocks the page for about seven seconds
when picked, almost all of it outside layout (building and patching 1850
nodes of vdom, the hulls, the score). A new pane width is a new stretch and
so a new solve.

## Left out

Authoring (place / connect / right-click menus, delete), probes and value
cards, the info panel, livelits, the definition editor, the toolbar (replay,
avatar look switch; the layout lab's menu is the page's two selects), `CanvasTrajectory` record/replay,
`CanvasFocus`, `CanvasProbe`, `CanvasValue`, `CanvasDefPreview`,
`CanvasReplayView`, `CanvasLayoutStudy`, `CanvasPresentation` (its
definition-by-definition splitting of one edit into several beats: here the
producer decides how finely to push), data-flow pulses (they need evaluation
samples), picking the avatar up, settings persistence.

## What the reader derives

The JSON is decoded in one place, `Graph.of_json` (`graph/graph.re`). The
canvas takes the typed record: `CanvasGraph.of_graph: Graph.t => t`, and
`of_json` / `of_string` are the two in a row. A snapshot that is not a graph
record (a missing field, an unknown kind) is refused whole and logged; the
canvas's own JSON reader, now gone, filled in defaults. That reader also took
`deps`, `hidden_deps` and `sat: {anchor, output}` from a node's JSON when a
producer supplied them. They are not in the record, nothing wrote them, and
they are no longer read.

Portolan's graph record does not carry four things the layout and score read.
`of_graph` derives them the way Hazel's extraction does (a `former` the record
gives is kept):

- **satellites:** a builtin (or a type variable) that is a function's only
  argument, its result, or a component is split into one terminal per use-site
  (`int@area`);
- **`deps`** of a named type: the named types its printed body mentions;
- **`former`:** `"()"` for a type with parts, `"[]"` for `x list`, `"+"` for a sum;
- **module formers** (`{}@M`): one node per module path in use, the host its
  members cluster on;
- a product's label becomes `()`.

It also decides how the record's unnamed types are drawn, since OCaml has many
more of them than Hazel:

- **former dots:** an unnamed tuple, list or option (a `derived` node whose
  `former` is `()`, `[]` or `?`) that a function uses or another type is made
  of is drawn as Hazel draws a tuple written inline: a `Product` dot with its
  glyph, fed by formation lines from its parts. Its text is in the tooltip;
- **text nodes:** every other unnamed type (a function type, a type applied to
  arguments, an inferred variant) keeps its text, shortened by `Abbreviate`
  (variants to their first tags, module paths to their last module, inner
  brackets closed up, 32 characters at most). The whole type is in the tooltip,
  through `n_doc`. The layout draws no formation lines into a `Derived` node,
  so a text node shows no parts;
- **value captions:** a node that is only there because values have that type
  is labelled with their names (`events: Dynarray.t(string)`);
- **outside types** (`ghost`) show no parts of their own, and a node nothing in
  the picture uses is dropped: the page's one-module view sends other modules'
  records along with their fields;
- a record's field names are not passed as `ctrs`, so its tooltip reads
  `vec = { x : int; y : int }`, not `vec = x + y`.

## Verified, and not

Verified in headless Chrome (puppeteer, 1200×800) against `demo.html`, with
both the dev and the optimised bundle: the seven demo steps play with no
console errors and no `SCORE:` or `ORPHAN:` journal lines. Exercised and looked
at in screenshots: additions, removals (ghosts, erase acts), look changes, an
error cause, a burst of six queued pushes, `reset` then `push`, `show`,
`setPace(false)`, `fit`, selection callbacks, a node drag and a hull collapse
(synthetic mouse events), bad JSON (logged, not thrown), and the dark theme in
`demo.html`. The extractor's real output for `examples/vector` was read and
drawn.

The layouts and edge styles were each drawn on Portolan's own modules in
headless Chrome and looked at (`site/studies/2026-11-11-layout-options/`);
the two selects were driven in the page, and a switch left no `JUMP:`,
`SCORE:` or `ORPHAN:` line in the journal. None of the new options has been
watched through a paced run, or dragged in by hand.

Known in the journal: one `JUMP: canvas-avatar` line (32–80 px) on a beat that
starts while the avatar is already embodied and thinking.

`CanvasGraph`'s rules (terminals, former dots, captions, what is left out,
shortened labels) have tests in `test/test_canvas_graph.re`, on graph records
a few items long. When the canvas stopped decoding JSON itself, the whole
`CanvasGraph.t` of 127 graphs (every module view of Portolan and of the
dungeon example, both recorded runs, the examples, the demo steps) was
compared before and after, and was equal.

Not verified: Firefox and Safari; touch input and pinch zoom; dragging and
collapsing by a real hand; graphs past a few dozen definitions. Hazel's
`Test_CanvasScore` tests were not ported.
