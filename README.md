# Portolan

*Working name.* A canvas for reading OCaml code bases at a level above the text, and for working in them alongside agents. It starts from the Constellation canvas in [Hazel](https://github.com/hazelgrove/hazel/pull/2505).

A portolan is a sailor's chart: coasts drawn in detail from the routes actually sailed, interiors left blank.

The first build runs from source. The notebook is at https://andrewblinn.com/portolan/, published from `site/` on every push to `main`; it includes a replay of a recorded run.

## Run it

Needs an opam switch with OCaml 5.2, dune, yojson and ppx_inline_test (set `PORTOLAN_SWITCH`; the default is `editor-output`), and a signed-in `claude` command for the agent.

```bash
./portolan new my-run
```

Then open http://127.0.0.1:8791/ and type a prompt (the ones used so far are in `prompts/`). The agent works in `local/runs/my-run`. Other forms:

```bash
./portolan serve path/to/a/dune/project      # watch an existing project
./portolan new my-run --nudge prompts/nudge-small-steps.md
./portolan new my-run --agent script:sidecar/scripts/vector.json   # a scripted stand-in, no login needed
./portolan replay local/runs/my-run/.portolan/trace-….jsonl --speed 4
```

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
| `sidecar/` | Watches a project, builds and tests it, runs the agent and relays its stream, serves the page, records a trace. |
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
