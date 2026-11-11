# The canvas and its upstream

This directory is Hazel's Constellation canvas running outside Hazel: a
js_of_ocaml bundle that draws graph snapshots (`graph/graph.ml`'s JSON) and
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
| `upstream/CanvasRipple.re` | `src/web/app/canvas/` | verbatim | |
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
| `CanvasGraph.re` | `src/web/app/canvas/CanvasGraph.re` | rewritten | Upstream's record types with the same field names (so the copies read them unchanged), ids as strings, terms as source text, plus `handles`. Extraction (lines 160–1606 upstream) is gone; `of_json` reads Portolan's Graph JSON instead. |
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
| `compact` | Portolan | The same layers, not stretched; separate pieces packed in source order into rows shaped like the pane. |
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

Portolan's graph record does not carry four things the layout and score read.
`CanvasGraph.of_json` takes them from the JSON when present (optional node
fields `deps`, `hidden_deps`, `sat: {anchor, output}`, `former`) and otherwise
derives them the way Hazel's extraction does:

- **satellites:** a builtin that is a function's only argument, its result, or a
  component of a product is split into one terminal per use-site (`int@area`);
- **`deps`** of a named type: the named types its printed body mentions;
- **`former`:** `"()"` for a type with parts, `"[]"` for `x list`, `"+"` for a sum;
- **module formers** (`{}@M`): one node per module path in use, the host its
  members cluster on;
- a product's label becomes `()`.

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

Known in the journal: one `JUMP: canvas-avatar` line (32–80 px) on a beat that
starts while the avatar is already embodied and thinking.

Not verified: Firefox and Safari; touch input and pinch zoom; dragging and
collapsing by a real hand; graphs past a few dozen definitions. Hazel's
`Test_CanvasScore` tests were not ported.
