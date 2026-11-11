/* The scripted stand-in for Claude Code: plays the steps of a JSON file
   without any model, doing the file operations itself and reporting to a
   [Turn] events shaped like Claude Code's. For demonstrations and for
   trying the page. */

/** A script to play in one project. */
type t;

/** [file] holds `{"steps": [STEP, …]}`. It is read each time it is
    played. Paths in its steps are relative to the project. */
let make: (~file: string, Project.t, Turn.t) => t;

/** A step of a script. */
type step =
  | /** `{"say": TEXT}`: the agent says this. */
    Say(string)
  | /** `{"wait": MS}`: nothing happens for so many milliseconds. */
    Wait(int)
  | /** `{"write": {file, content}}`: the file is written. */
    Write({
      file: string,
      content: string,
    })
  | /** `{"edit": {file, old, new}}`: the first [old] in the file is
        replaced by [by]. */
    Edit({
      file: string,
      old: string,
      by: string,
    })
  | /** `{"ask": {tool, input}}`: the agent asks to use the tool, or, with
        the tool AskUserQuestion, puts its questions, and waits for the
        user as Claude Code would. */
    Ask({
      tool: string,
      input: Yojson.Safe.t,
    });

/** The step a JSON value spells; [None] for anything else, which a
    script skips. Raises [Yojson.Safe.Util.Type_error] for a `write`,
    `edit` or `ask` with a part missing. */
let step_of_json: Yojson.Safe.t => option(step);

/** [replace(~old, ~by, text)] is [text] with its first [old] replaced by
    [by]; [None] if [old] is not in it. */
let replace: (~old: string, ~by: string, string) => option(string);

/** Plays the script from its start as one turn, on a thread of its own.
    A script that cannot be read or goes wrong ends the turn with an
    [Error] event. */
let play: t => unit;

/** The user's answer to the request [id] of the script's. */
let answer: (t, ~id: string, Event.answer) => unit;

/** Ends the turn under way after the step it is at. */
let stop: t => unit;

/** Ends the turn under way without a word. */
let forget: t => unit;
