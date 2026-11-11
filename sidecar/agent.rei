/* The coding agent: Claude Code in its headless streaming mode, or a
   scripted stand-in that plays steps from a file. Either way its stream
   goes onto the bus as `agent {ev}` events, where `ev` is an object
   shaped like Claude Code's stream output, with `agent_state {state}`
   ("running", "idle", "exited") around them and `agent_raw {line}` for
   output that is not JSON. */

/** Which agent answers prompts. */
type kind =
  | /** Claude Code, run as `claude -p` in the project. */
    Claude
  | /** The steps in this JSON file, played without any model. */
    Script(
      string,
    );

/** ["claude"] is [Claude]; ["script:FILE"] is [Script("FILE")]. */
let kind_of_string: string => option(kind);

/** An agent working in one project. */
type t;

/** Sets up the agent for a project; nothing is started until the first
    prompt.

    [model] is passed to Claude Code if given. [instructions] are files
    whose text is added to Claude Code's system prompt (missing files are
    skipped). [allow] are Claude Code allowed-tool patterns, such as
    [Bash(make:*)], pre-approved on top of [default_allow].

    Claude Code runs in the project's switch environment. If it has held a
    conversation in this project before (the sidecar notes this in the
    project's state folder), the first prompt continues that conversation. */
let make:
  (
    ~kind: kind,
    ~model: string=?,
    ~instructions: list(string),
    ~allow: list(string),
    Project.t
  ) =>
  t;

/** The tool patterns Claude Code may use without asking: every `dune`
    command and read-only shell helpers. Edits to files are accepted too. */
let default_allow: list(string);

/** Sends the user's prompt to the agent, starting it if needed, and puts
    a `prompt {text}` event on the bus. Returns at once. */
let prompt: (t, string) => unit;

/** Asks a running Claude Code process to stop. */
let stop: t => unit;

/** What the agent last did to the project's files, as a graph event's
    `cause`, and forgets it; if the agent did nothing since the last call,
    the cause is an edit by hand. */
let take_cause: t => Yojson.Safe.t;
