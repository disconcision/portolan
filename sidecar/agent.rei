/* The coding agent: Claude Code in its headless streaming mode, or a
   scripted stand-in that plays steps from a file. Either way its stream
   goes onto the bus as `agent {ev}` events, where `ev` is an object
   shaped like Claude Code's stream output, with `agent_state {state}`
   ("running", "waiting" for an answer from the user, "idle", "exited")
   whenever the state changes, and `agent_raw {line}` for output that is
   not JSON.

   When the agent wants to use a tool that nothing allows, or has a
   question for the user, it waits, and the bus gets
   `ask {id, tool, input, patterns, tool_use_id}`: the tool and its input
   as Claude Code gives them (the tool AskUserQuestion carries
   `questions`, each with `options`), and the patterns "always allow"
   would remember. The answer is given with [answer] and reported as
   `answer {id, decision}`, where the decision is "once", "always" (with
   `patterns`), "deny", "answered" (with `answers`), or "cancelled" when
   the turn ended before anyone answered.

   Claude Code is driven over its stream-json control protocol
   (`--permission-prompt-tool stdio`): requests to use a tool arrive as
   `control_request` lines on its output and are answered on its input,
   and a turn is stopped with an `interrupt` request. */

/** Which agent answers prompts. */
type kind =
  | /** Claude Code, run as `claude -p` in the project. */
    Claude
  | /** The steps in this JSON file, played without any model. A step is
        `{"say": TEXT}`, `{"write": {file, content}}`,
        `{"edit": {file, old, new}}`, `{"wait": MS}` or
        `{"ask": {tool, input}}`, which waits for the user as Claude
        Code would. */
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
    [Bash(make:*)], pre-approved on top of [default_allow] and of the
    patterns remembered for the project (see [Permissions]).

    Claude Code runs in the project's switch environment. If it has held a
    conversation in this project before (the sidecar notes its id in the
    project's state folder), the first prompt carries that one on. */
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

/** A definition a prompt is about: its name, and the file and first and
    last line it is at, if known. */
type about = {
  name: string,
  where: option((string, int, int)),
};

/** Sends the user's prompt to the agent, starting it if needed, and puts
    a `prompt {text, about}` event on the bus. With [about], the agent is
    also told which definition the user had selected. Returns at once. */
let prompt: (t, ~about: about=?, string) => unit;

/** The user's answer to a request the agent is waiting on. */
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

/** Answers the request with this id. Does nothing if it is not waiting
    (already answered, or its turn has ended). */
let answer: (t, ~id: string, answer) => unit;

/** Stops the turn under way: the agent ends it and stays ready for the
    next prompt, with the conversation intact. A Claude Code process that
    has not ended its turn a few seconds later is killed; the next prompt
    then starts a new process on the same conversation. */
let stop: t => unit;

/** Ends the conversation: the agent is stopped without a word and the
    next prompt starts a conversation with no memory of this one. */
let forget: t => unit;

/** What the agent last did to the project's files, as a graph event's
    `cause`, and forgets it; if the agent did nothing since the last call,
    the cause is an edit by hand. */
let take_cause: t => Yojson.Safe.t;
