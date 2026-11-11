/* Where an agent's turn stands, whichever kind of agent it is: its state
   as the page shows it, and the requests waiting for the user's answer.
   Whatever drives the agent (Claude Code's output, a script, the user's
   answers) says here what happened; [step] decides what that changes and
   which events report it (see [Event]: [Agent], [Agent_state], [Ask],
   [Answer]). */

/** Where things stand. A value: [step] gives the next one. */
type standing;

/** Before any turn: idle, nothing waiting. */
let idle: standing;

/** Something that happens in an agent's turns. */
type happening =
  | /** A turn is under way. */
    Began
  | /** An event of the agent's stream, shaped as Claude Code writes it. */
    Told(
      Yojson.Safe.t,
    )
  | /** The turn is over, with this last event of its stream. */
    Ended(
      Yojson.Safe.t,
    )
  | /** The agent waits for the user's answer to this request. */
    Asked(
      Protocol.request,
    )
  | /** The agent no longer wants an answer to the request with this id. */
    Withdrew(
      string,
    )
  | /** The user answered the request with this id. */
    Answered(
      string,
      Event.answer,
    )
  | /** The agent's process has gone. */
    Exited
  | /** The conversation is being forgotten: back to before any turn,
        without a word. */
    Forgotten;

/** Where things stand after something happened, and the events that
    report it, in order.

    The state is reported only when it changes. A request makes the agent
    wait; it is at work again once the last waiting request is answered.
    Requests still waiting when a turn ends or the process goes are
    reported cancelled, oldest first, as is one the agent withdraws. An
    answer or a withdrawal for a request that is not waiting changes and
    reports nothing. */
let step: (standing, happening) => (standing, list(Event.t));

let state: standing => Event.state;

/** How many turns have ended, so a stop can tell whether its turn did. */
let finished: standing => int;

/** The input of the tool that the waiting request with this id is for. */
let waiting: (standing, string) => option(Yojson.Safe.t);

/** An agent's turns in one project, as they happen. */
type t;

let make: Project.t => t;

/** Takes the step, and puts its events on the bus. A stream event that
    uses a tool changing the project's files is noted for [take_cause]. */
let happened: (t, happening) => unit;

/** The user's answer to the request with this id: [happened] with
    [Answered], giving the input of the tool the request was for; [None],
    with nothing done, if no such request is waiting. */
let answered: (t, ~id: string, Event.answer) => option(Yojson.Safe.t);

let standing: t => standing;

/** What the agent last did to the project's files, and forgets it; if
    it did nothing since the last call, the cause is an edit by hand. */
let take_cause: t => Event.cause;
