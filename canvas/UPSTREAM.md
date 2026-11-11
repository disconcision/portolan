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

Upstream's console testers (`__canvasBurst`, `__canvasStage`, `__cameraState`,
...) are still installed.

## Files

`upstream/` is byte-for-byte; `edited/` is upstream plus edits marked
`PORTOLAN`; `shim/` stands in for Hazel modules the copies mention; the rest
is new.

| File here | Upstream path | Status | Why |
|---|---|---|---|
| `upstream/GraphLayout.re`, `.rei` | `src/util/` | verbatim | |
| `upstream/AgentPulse.re` | `src/util/` | verbatim | |
| `upstream/PerfTimer.re` | `src/util/` | verbatim | |
| `upstream/OptUtil.re` | `src/util/` | verbatim | |
| `upstream/Animation.re` | `src/web/` | verbatim | |
| `upstream/CanvasScore.re` | `src/web/app/canvas/` | verbatim | |
| `upstream/CanvasLayout.re` | `src/web/app/canvas/` | verbatim | |
| `upstream/CanvasLayoutExperiments.re` | `src/web/app/canvas/` | verbatim | `CanvasLayout` needs it (wire routing, the layout lab). Only the default "current" layout with curved wires is reachable here. |
| `upstream/CanvasZoom.re` | `src/web/app/canvas/` | verbatim | |
| `upstream/CanvasEnact.re` | `src/web/app/canvas/` | verbatim | |
| `upstream/CanvasAvatar.re` | `src/web/app/canvas/` | verbatim | |
| `upstream/CanvasBubble.re` | `src/web/app/canvas/` | verbatim | |
| `upstream/CanvasCamera.re` | `src/web/app/canvas/` | verbatim | |
| `upstream/CanvasRipple.re` | `src/web/app/canvas/` | verbatim | |
| `upstream/CanvasJelly.re` | `src/web/app/canvas/` | verbatim | |
| `upstream/CanvasLog.re` | `src/web/app/canvas/` | verbatim | |
| `edited/CanvasView.re` | `src/web/app/canvas/` | edited (the hull palette, 8 lines) | The six hull colours read `var(--pc-hull-N, <upstream colour>)` so a host can theme them. Worth taking upstream. |
| `edited/CanvasBuffer.re` | `src/web/app/canvas/` | edited (about 25 lines) | The beat's payload is a graph snapshot instead of Hazel's editor model: `type model = CanvasGraph.t`; `same_program` is graph equality; no "statics pending" state. Dwell rules, queue, hold, staging: untouched. |
| `shim/Util.re` | (`util` library) | shim | Re-exports `JsUtil`, `WebUtil`, `OptUtil`, `PerfTimer`, `GraphLayout`, `AgentPulse`, `Key` under `Util.`, as the copies spell them. |
| `shim/JsUtil.re` | `src/util/JsUtil.re` | shim | 4 of its functions, upstream's bodies (`cached_client_height` without the cache). |
| `shim/WebUtil.re` | `src/util/WebUtil.re` | shim | `clss`, `Node`, `Attr`. |
| `shim/Key.re` | `src/util/Key.re` | shim | `get_key`. |
| `shim/Haz3lcore.re` | (`haz3lcore`) | shim | `Id.t = string`: `CanvasView` names `Haz3lcore.Id.t` in five signatures. |
| `shim/CanvasTrajectory.re` | `src/web/app/canvas/CanvasTrajectory.re` | shim | Two hooks (`on_change`, `busy`) the copies call. |
| `CanvasGraph.re` | `src/web/app/canvas/CanvasGraph.re` | rewritten | Upstream's record types with the same field names (so the copies read them unchanged), ids as strings, terms as source text, plus `handles`. Extraction (lines 160–1606 upstream) is gone; `of_json` reads Portolan's Graph JSON instead. |
| `CanvasDirector.re` | `src/web/app/canvas/CanvasSidebar.re` | lifted + new | See below. |
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

## Left out

Authoring (place / connect / right-click menus, delete), probes and value
cards, the info panel, livelits, the definition editor, the toolbar (layout
lab, replay, avatar look switch), `CanvasTrajectory` record/replay,
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

Known in the journal: one `JUMP: canvas-avatar` line (32–80 px) on a beat that
starts while the avatar is already embodied and thinking.

Not verified: Firefox and Safari; touch input and pinch zoom; dragging and
collapsing by a real hand; graphs past a few dozen definitions. Hazel's
`Test_CanvasScore` tests were not ported.
