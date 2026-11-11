/* The coding agent: Claude Code in its headless streaming mode, or a
   scripted stand-in that plays steps from a file. Either way its stream
   goes onto the bus (see [Event]) as [Agent] events, each an object
   shaped like Claude Code's stream output, with [Agent_state] whenever
   the state changes, and [Agent_raw] for output that is not JSON.

   When the agent wants to use a tool that nothing allows, or has a
   question for the user, it waits, and the bus gets an [Ask]: the tool
   and its input as Claude Code gives them (the tool AskUserQuestion
   carries `questions`, each with `options`), and the patterns "always
   allow" would remember. The answer is given with [answer] and reported
   as an [Answer], which is also sent, as cancelled, when the turn ended
   before anyone answered.

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

/** Sends the user's prompt to the agent, starting it if needed, and puts
    a [Prompt] event on the bus. With [about], the agent is also told
    which definition the user had selected. Returns at once. */
let prompt: (t, ~about: Event.about=?, string) => unit;

/** Answers the request with this id. Does nothing if it is not waiting
    (already answered, or its turn has ended). */
let answer: (t, ~id: string, Event.answer) => unit;

/** Stops the turn under way: the agent ends it and stays ready for the
    next prompt, with the conversation intact. A Claude Code process that
    has not ended its turn a few seconds later is killed; the next prompt
    then starts a new process on the same conversation. */
let stop: t => unit;

/** Ends the conversation: the agent is stopped without a word and the
    next prompt starts a conversation with no memory of this one. */
let forget: t => unit;

/** What the agent last did to the project's files, and forgets it; if
    the agent did nothing since the last call, the cause is an edit by
    hand. */
let take_cause: t => Event.cause;
