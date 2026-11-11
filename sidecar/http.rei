/* The web server: the page, the canvas bundle, the event stream, and the
   few requests the page makes of the sidecar.

     GET  /                 the page (`web/index.html`); other paths are
                            files under `web/`
     GET  /canvas/FILE      the canvas bundle, from `canvas/dist/`
     GET  /events           server-sent events: each `data:` line is one
                            bus event, plus `caught_up` once the backlog
                            has been sent (and `replay_end` in a replay);
                            of the backlog's graphs only the newest is sent
     GET  /trace            every event so far, one JSON object per line
     GET  /source?file=REL  a project file, as text
     GET  /refmt?file=REL&line=N&end_line=M&to=re|ml
                            lines N to M of a project file in the asked
                            syntax; if they could not be converted, the
                            lines as written, with the header
                            `X-Portolan-Converted: no`
     POST /types            `{"types": [TEXT, …]}`: printed OCaml types;
                            answers `{"types": […]}`, each in Reason
                            syntax, or null where it could not be read
     POST /prompt           `{"text": …}`: a prompt for the agent
     POST /stop             stop the agent */

/** What the server shows. */
type source =
  | /** A project being watched, and the agent working in it. */
    Live(
      Project.t,
      Agent.t,
    )
  | /** A recorded trace's events, played to each viewer from the start
        with the recording's own timing, [speed] times as fast. There is
        no project, so no sources, prompts or stops. */
    Replay({
      lines: list(string),
      speed: float,
    });

/** Serves on the loopback address at [port] until the process ends.
    [home] is the Portolan folder holding `web/` and `canvas/dist/`. */
let serve: (~port: int, ~home: string, source) => unit;
