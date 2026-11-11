/* The events the sidecar reports: what goes on the bus, to the page and
   into a trace file. This is the one place their JSON is written and
   read; the page reads the same shapes (`handlers` in `web/app.js`). */

/** What the agent is doing: at work, waiting for the user's answer,
    between turns, or gone (its process ended). */
type state =
  | Running
  | Waiting
  | Idle
  | Exited;

/** A definition a prompt is about: its name, and the file and first and
    last line it is at, if known. */
type about = {
  name: string,
  where: option((string, int, int)),
};

/** What led to a new graph. */
type cause =
  | /** Someone edited the files; nothing the agent did since the last graph. */
    By_hand
  | /** The agent's tool ([Write], [Edit], [MultiEdit]) changed this file. */
    Wrote({
      tool: string,
      file: string,
    })
  | /** The agent ran this shell command. */
    Ran(string)
  | /** The tests ran, and their results changed. */
    Tested
  | /** Nothing changed: the graph opens a new trace. */
    Carried_over;

/** How a graph event carries the graph: whole, or as the change from the
    graph of the event before it (see [Delta]). */
type drawing =
  | Whole(Yojson.Safe.t)
  | Change(Yojson.Safe.t);

/** The user's answer to a request of the agent's. */
type answer =
  | /** This one use may go ahead. */
    Once
  | /** It may go ahead, and these patterns are allowed from now on, in
        this project. */
    Always(
      list(string),
    )
  | /** It may not; the agent is told so. */
    Deny
  | /** The answers to the agent's questions: each question's text with
        the label chosen (several labels joined with ", "). */
    Answers(
      list((string, string)),
    );

/** An event. The comments give each one's ["type"] and fields on the wire. */
/** How a project is being shown. */
type mode =
  | /** A sidecar is watching it. */
    Live
  | /** As it stood when it was extracted once (`portolan snapshot`): no
        run and no agent. */
    Snapshot;

type t =
  | /** `session {restart}`: a sidecar started; [restart] if the trace
        already held events. */
    Session({
      restart: bool,
    })
  | /** `hello {project, agent, mode, switch?}`: the project's name, the
        `--agent` flag as given, how the project is being shown, and the
        opam switch if not the sidecar's own. */
    Hello({
      project: string,
      agent: string,
      mode,
      opam_switch: option(string),
    })
  | /** `conversation {earlier}`: a new conversation; the one before is
        kept in this file, relative to `.portolan`. */
    Conversation({
      earlier: string,
    })
  | /** `build {ok, output, ms}` */
    Build({
      ok: bool,
      output: string,
      ms: int,
    })
  | /** `graph {graph | delta, cause, partial?}`; [partial] when the build
        failed and the graph is what still compiled. */
    Graph({
      drawing,
      cause,
      partial: bool,
    })
  | /** `tests {ok, output, failing: [{file, line}]}` */
    Tests({
      ok: bool,
      output: string,
      failing: list((string, int)),
    })
  | /** `extract {ok: false, output}`: the extractor failed on a good build. */
    Extract_failed(
      string,
    )
  | /** `error {where, message}`: a part of the sidecar went wrong. */
    Error({
      where: string,
      message: string,
    })
  | /** `prompt {text, about?: {name, file?, line?, end_line?}}` */
    Prompt({
      text: string,
      about: option(about),
    })
  | /** `agent {ev}`: one object of the agent's stream, shaped as Claude
        Code writes it. */
    Agent(
      Yojson.Safe.t,
    )
  | /** `agent_state {state}` */
    Agent_state(state)
  | /** `agent_raw {line}`: output of the agent's that is not JSON. */
    Agent_raw(
      string,
    )
  | /** `ask {id, tool, input, patterns, tool_use_id?}`: the agent waits
        for the user's leave to use [tool] with [input], or (the tool
        AskUserQuestion) for answers to the questions in [input].
        [patterns] are what "always allow" would remember. */
    Ask({
      id: string,
      tool: string,
      input: Yojson.Safe.t,
      patterns: list(string),
      tool_use: option(string),
    })
  | /** `answer {id, decision, patterns? | answers?}`: the request was
        answered ("once", "always", "deny", "answered"), or, with [None],
        "cancelled": its turn ended before anyone answered. */
    Answer({
      id: string,
      answer: option(answer),
    });

/** Which of the events one is. */
module Kind: {
  type t =
    | Session
    | Hello
    | Conversation
    | Build
    | Graph
    | Tests
    | Extract
    | Error
    | Prompt
    | Agent
    | Agent_state
    | Agent_raw
    | Ask
    | Answer;

  /** The kind's name on the wire, an event's ["type"]: ["agent_state"]. */
  let name: t => string;

  let of_name: string => option(t);
};

let kind: t => Kind.t;

/** The event as a JSON object: ["type"] first, then its fields. */
let to_json: t => Yojson.Safe.t;

/** Reads what [to_json] writes; [None] for anything else, such as an
    event of a kind this version does not know. */
let of_json: Yojson.Safe.t => option(t);

/** The page's `{name, file, line, end_line}`, as in a prompt event. */
let about_of_json: Yojson.Safe.t => option(about);

/** The page's `{decision, patterns | answers}`, as in an answer event;
    [None] for "cancelled" or anything else that is not the user's to give. */
let answer_of_json: Yojson.Safe.t => option(answer);

/** Cuts a string to at most [n] bytes (6000 unless given), so one long
    output does not swamp an event. */
let clip: (~n: int=?, string) => string;

/** An event as it is kept and sent: one line of JSON text. */
module Line: {
  type event := t;

  /** A line of a trace. One made here is an object with ["t"]
      (milliseconds on the trace's clock) first and ["type"] second, so
      the kind can be read off its front without parsing a long line; one
      read from a file is whatever was there. */
  type t;

  /** The event, as it happened at [time]. */
  let make: (~time: int, event) => t;

  /** A line read from a trace file, unchecked. */
  let of_string: string => t;
  let to_string: t => string;

  /** Whether the line looks like a whole object. A sidecar killed
      mid-write leaves a cut one. */
  let is_whole: t => bool;

  /** The event's kind, read off the front; [None] if it is not there or
      not a kind this version knows. */
  let kind: t => option(Kind.t);

  /** The event's time. */
  let time: t => option(int);

  /** The event, if it is one [of_json] reads. */
  let event: t => option(event);

  /** For a graph event, whether it holds the graph whole or a change,
      read off the front. */
  let draws:
    t =>
    option(
      [
        | `Whole
        | `Change
      ],
    );

  /** The graph or change a line holds. Needs only that field, so it also
      reads graph events whose other fields are not as this version
      writes them. */
  let drawing: t => option(drawing);

  /** The line with its graph or change replaced, the rest as written. */
  let redraw: (t, drawing) => t;
};
