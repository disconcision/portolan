/* Claude Code as the agent: run headless in the project, started at the
   first prompt and kept across turns. Its output is read with [Protocol]
   and reported to a [Turn]; prompts, answers and stops go to its input.

   Claude Code runs in the project's switch environment. If it has held a
   conversation in this project before (the sidecar notes its session id
   in the project's state folder), a new process carries that one on. */

/** Claude Code in one project: its settings, its process if one is
    running, and the conversation to carry on. */
type t;

/** The tool patterns Claude Code may use without asking: every `dune`
    command and read-only shell helpers. Edits to files are accepted too. */
let default_allow: list(string);

/** Nothing is started until the first prompt.

    [model] is passed to Claude Code if given. [instructions] are files
    whose text is added to its system prompt (missing files are skipped).
    [allow] are allowed-tool patterns, such as [Bash(make:*)], pre-approved
    on top of [default_allow] and of the patterns remembered for the
    project (see [Permissions]). */
let make:
  (
    ~model: string=?,
    ~instructions: list(string),
    ~allow: list(string),
    Project.t,
    Turn.t
  ) =>
  t;

/** The shell command that starts Claude Code in the folder [root],
    speaking stream-json both ways, accepting edits, and asking on its
    output before using a tool that [allow] does not cover. With [resume]
    it carries on that session; [instructions], unless empty, are added to
    its system prompt. */
let command:
  (
    ~root: string,
    ~resume: option(string),
    ~model: option(string),
    ~instructions: string,
    ~allow: list(string)
  ) =>
  string;

/** Sends a prompt, starting Claude Code if it is not running. */
let prompt: (t, string) => unit;

/** Sends the user's answer to the request [id], which asked to use a
    tool with [input]. */
let answer: (t, ~id: string, ~input: Yojson.Safe.t, Event.answer) => unit;

/** Asks Claude Code to end the turn under way, if there is one; it stays
    ready for the next prompt, with the conversation intact. A process
    that has not ended the turn a few seconds later is killed; the next
    prompt then starts a new one on the same conversation. */
let stop: t => unit;

/** Ends the conversation: the process is killed, and the next prompt
    starts one with no memory of this one. */
let forget: t => unit;
