# Portolan: working here

Portolan (working name) is a canvas for reading OCaml code bases above the level of text, and for working in them with agents. It starts from Hazel's Constellation canvas. The repo is public.

## Layout

`site/` is the notebook, served locally on port 5191 and published to https://andrewblinn.com/portolan/ on every push to `main`. `site/index.html` is the front page and the map of everything.

- **Prototypes** are runnable things. A web app anyone can open gets a link; anything else links to its source.
- **Living documents** (`site/notes/`) are kept current. `scope.html` is the scope of the project.
- **Studies** (`site/studies/`) are written once and left as they were. Name them `YYYY-MM-DD-what.html`.

## The scope notes

`site/notes/scope.html` records the scope as it is talked through. When a new direction comes up:

- add a row to the directions index, saying where it came from (Andrew, or proposed by an agent);
- add a short sketch under Directions;
- add a row to the tandem tracker, with its state in Hazel and here;
- update the date in the eyebrow and the `updated` field of its entry in `site/index.html`.

Keep it short. Cut before adding. The first build stays small: only what Hazel's canvas has now that translates to OCaml.

The file has no `<html>`, `<head>` or `<body>` tags on purpose, so the same file can also be published as a claude.ai artifact.

## Where new work goes

- **A write-up, comparison or mock-up page:** a study, self-contained, using `../assets/notebook.css`.
- **Something to keep current:** a living document in `site/notes/`.
- **The front page:** add every new page to `ENTRIES` in `site/index.html`, with a one-line plain description (what it is, no pitch). Link only pages in the repo or public URLs, never claude.ai artifacts: those are private.
- **Big or throwaway files:** a `local/` folder. Git ignores it.

## Pages

- Standalone HTML, no build step. Fonts from Google Fonts; everything else in the repo.
- Both light and dark themes, through the tokens in `notebook.css`.
- Say what was verified and what was not. A study built from agent research says so at the top.
- This repo is public. No private paths, tokens or personal details in pages.

## Commits

Small, scoped commits; the message says what changed and why. Commit only the files you changed. Push after committing: a push to `main` publishes the site.
