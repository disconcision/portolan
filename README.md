# Portolan

*Working name.* A canvas for reading OCaml code bases at a level above the text, and for working in them alongside agents. It starts from the Constellation canvas in [Hazel](https://github.com/hazelgrove/hazel/pull/2505).

A portolan is a sailor's chart: coasts drawn in detail from the routes actually sailed, interiors left blank.

The first build runs from source. The notebook is at https://andrewblinn.com/portolan/, published from `site/` on every push to `main`; it includes a replay of a recorded run.

## Run it

Portolan itself needs an opam switch with OCaml 5.2, dune, yojson, reason and ppx_inline_test (set `PORTOLAN_SWITCH`; the default is `editor-output`), and a signed-in `claude` command for the agent.

```bash
./portolan new my-run
```

Then open http://127.0.0.1:8791/ and type a prompt (the ones used so far are in `prompts/`). The agent works in `local/runs/my-run`. Other forms:

```bash
./portolan serve path/to/a/dune/project      # an existing project; `./portolan serve .` is Portolan on itself
./portolan new my-run --nudge prompts/nudge-small-steps.md
./portolan new my-run --agent script:sidecar/scripts/vector.json   # a scripted stand-in, no login needed
./portolan replay path/to/project/.portolan/trace.jsonl --speed 4
./portolan new my-run --agent script:sidecar/scripts/asking.json   # the stand-in asking for leave and asking a question
./portolan help                              # every flag
```

What to expect:

- **The project's own state.** Portolan keeps a `.portolan/` folder (the trace, earlier conversations, the last graph, what you chose to always allow) and a `_build_portolan/` build folder in the project. Both ignore themselves; the project's `.gitignore` is not touched.
- **Restarts carry on.** The trace is one file per project, so stopping and starting again shows the earlier conversation, and Claude Code continues it.
- **Broken builds.** The modules that still compile are redrawn; in the others the definitions holding an error are marked and the rest stay as they were.
- **The project's switch.** Builds, tests and the agent's shell run in the project's own `_opam` if it has one, or the switch given with `--project-switch`. The compiler must be OCaml 5.2.x for now.
- **What the agent may do without asking.** Edit files, run any dune command, and read-only shell commands. Add more with `--allow`, for example `--allow 'Bash(make:*)'`. For anything else the agent waits and the chat shows the request with **Allow once**, **Always allow** and **Deny**. "Always allow" remembers the patterns shown under the buttons (edit them first if they are too wide or too narrow) in the project's `.portolan/allow`, one per line; delete a line there to take it back. A question the agent puts to you shows its options as buttons.
- **The chat.** Edits by the agent show as diffs, folded when long. Select a definition on the canvas and the next message carries it: the agent is told its name, file and lines (the chip above the message box; × takes it off). **stop** ends the agent's turn and keeps the conversation. **new** (click twice) starts a new conversation: the trace so far moves to `.portolan/conversations/<time>.jsonl`, which `./portolan replay` plays, and Claude Code starts without memory of it.
- **Trace size.** A graph event holds the change from the graph before it, not the whole graph; an hour on Portolan itself is about 1.5 MB. `./portolan compact old.jsonl new.jsonl` rewrites a trace recorded before this the same way.

`tools/summarize_trace.py <trace>` prints what happened in a run. `tools/scrub_trace.py` makes a trace safe to publish under `site/runs/`.

## The notebook, locally

```bash
python3 -m http.server 5191 --bind 127.0.0.1 --directory site
```

Then open http://127.0.0.1:5191/.

## What's where

| Path | What it is |
|---|---|
| `graph/` | **The graph record.** The only thing the canvas reads: type nodes, function edges, values, tests. |
| `extract/` | Reads a dune project's compiled annotations and produces the graph record. |
| `sidecar/` | Watches a project, builds and tests it, runs the agent and relays its stream, serves the page, records a trace. One small module per job, each with an interface (`.rei`) that says what it does. |
| `web/` | The page: chat beside the canvas, styled after Hazel. `view.js` is a plain fallback used only if the canvas bundle is missing. |
| `canvas/` | Hazel's Constellation canvas, ported. `UPSTREAM.md` lists every file against Hazel's. `dist/` is the built bundle the page loads; `./portolan build-canvas` rebuilds it. |
| `prompts/` | The Hazel demo prompts in OCaml wording, and the small-steps nudge. |
| `examples/` | Small dune projects used to test the extractor. |
| `tools/` | Trace summary and scrub scripts. |
| `site/index.html` | **The notebook front page.** Its `ENTRIES` list is the map: prototypes, living documents, studies. |
| `site/notes/` | Living documents, kept current. `scope.html` is the scope of the project: purpose, first build, directions. |
| `site/studies/` | One-off studies, dated in the file name and left as written. |
| `site/assets/notebook.css` | Styles shared by the front page and the studies. |
| `.github/workflows/pages.yml` | Publishes `site/` to GitHub Pages. |

Conventions for agents and people working here are in `CLAUDE.md`.
