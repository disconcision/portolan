# Portolan

*Working name.* A canvas for reading OCaml code bases at a level above the text, and for working in them alongside agents. It starts from the Constellation canvas in [Hazel](https://github.com/hazelgrove/hazel/pull/2505).

A portolan is a sailor's chart: coasts drawn in detail from the routes actually sailed, interiors left blank.

Nothing is built yet. The notebook is at https://andrewblinn.com/portolan/, published from `site/` on every push to `main`.

## Run the notebook locally

```bash
python3 -m http.server 5191 --bind 127.0.0.1 --directory site
```

Then open http://127.0.0.1:5191/.

## What's where

| Path | What it is |
|---|---|
| `site/index.html` | **The notebook front page.** Its `ENTRIES` list is the map: prototypes, living documents, studies. |
| `site/notes/` | Living documents, kept current. `scope.html` is the scope of the project: purpose, first build, directions. |
| `site/studies/` | One-off studies, dated in the file name and left as written. |
| `site/assets/notebook.css` | Styles shared by the front page and the studies. |
| `.github/workflows/pages.yml` | Publishes `site/` to GitHub Pages. |

Conventions for agents and people working here are in `CLAUDE.md`.
