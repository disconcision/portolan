/* Claude Code's stream-json protocol, as far as Portolan speaks it: what
   Claude Code writes on its output, one JSON object a line, and what its
   host writes on its input. With `--permission-prompt-tool stdio` the
   requests to use a tool arrive on the output as `control_request` lines
   and are answered on the input.

   Nothing here does anything: lines are read into values, and values
   written as lines. */

/** Claude Code asks its host before using a tool nothing has allowed,
    and to put its own questions to the user (the same request, for the
    tool AskUserQuestion, whose input carries `questions`). */
type request = {
  id: string,
  tool: string,
  input: Yojson.Safe.t,
  /** The rules Claude Code proposes to allow from now on (its
      `permission_suggestions`); null if it has none. */
  suggestions: Yojson.Safe.t,
  /** The tool use in the stream that the request is for. */
  tool_use: option(string),
};

/** What a line of Claude Code's output says. */
type said =
  | /** A turn begins (`system`, `init`): the event, and the session Claude
        Code announces with it. */
    Began(
      Yojson.Safe.t,
      option(string),
    )
  | /** The turn is over (`result`). */
    Ended(Yojson.Safe.t)
  | /** Any other event of the stream. */
    Told(Yojson.Safe.t)
  | /** A request to use a tool (`can_use_tool`). */
    Asked(request)
  | /** A request of a kind Portolan does not handle, by its id; it is
        owed a [refusal]. */
    Asked_other(
      string,
    )
  | /** The request with this id is withdrawn, for instance because the
        turn was stopped. */
    Withdrew(
      string,
    )
  | /** Nothing to pass on: the acknowledgement of a request of ours, a
        request too malformed to answer, a blank line. */
    Nothing
  | /** Output that is not a JSON object. */
    Raw(string);

let read: string => said;

/** What an event of the stream is about to do to the project's files, if
    anything: the last file-changing tool use it holds. [relative] makes a
    path relative to the project, if it is inside it. Commands are cut to
    120 bytes. */
let cause:
  (~relative: string => option(string), Yojson.Safe.t) => option(Event.cause);

/** A line for Claude Code's input. */
type message;

/** The line's text, without its newline. */
let to_string: message => string;

/** The user's prompt. */
let prompt: string => message;

/** The answer to the request [id], which asked to use a tool with
    [input]. [Always] allows its patterns for the rest of the process's
    session; [Answers] go back as the input's `answers`. */
let answer: (~id: string, ~input: Yojson.Safe.t, Event.answer) => message;

/** Tells Claude Code its request [id] is not one Portolan handles. */
let refusal: (~id: string) => message;

/** Asks Claude Code to end the turn under way. [id] names the request. */
let interrupt: (~id: string) => message;
