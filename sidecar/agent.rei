/* The coding agent the user talks to: Claude Code (see [Claude]), or a
   scripted stand-in that plays steps from a file (see [Script]). Either
   way its stream goes onto the bus (see [Event]) as [Agent] events, each
   an object shaped like Claude Code's stream output, with [Agent_state]
   whenever the state changes, and [Agent_raw] for output that is not
   JSON.

   When the agent wants to use a tool that nothing allows, or has a
   question for the user, it waits, and the bus gets an [Ask]: the tool
   and its input as Claude Code gives them (the tool AskUserQuestion
   carries `questions`, each with `options`), and the patterns "always
   allow" would remember. The answer is given with [answer] and reported
   as an [Answer], which is also sent, as cancelled, when the turn ended
   before anyone answered. [Turn] keeps track of all that. */

/** Which agent answers prompts. */
type kind =
  | /** Claude Code, run as `claude -p` in the project. */
    Claude
  | /** The steps in this JSON file, played without any model (see
        [Script.step]). */
    Script(
      string,
    );

/** ["claude"] is [Claude]; ["script:FILE"] is [Script("FILE")]. */
let kind_of_string: string => option(kind);

/** An agent working in one project. */
type t;

/** Sets up the agent for a project; nothing is started until the first
    prompt. [model], [instructions] and [allow] are for Claude Code (see
    [Claude.make]). */
let make:
  (
    ~kind: kind,
    ~model: string=?,
    ~instructions: list(string),
    ~allow: list(string),
    Project.t
  ) =>
  t;

/** Sends the user's prompt to the agent, starting it if needed, and puts
    a [Prompt] event on the bus. With [about], the agent is also told
    which definition the user had selected. Returns at once. */
let prompt: (t, ~about: Event.about=?, string) => unit;

/** Answers the request with this id. Does nothing if it is not waiting
    (already answered, or its turn has ended). With [Always], the
    patterns are remembered for the project (see [Permissions]). */
let answer: (t, ~id: string, Event.answer) => unit;

/** Stops the turn under way: the agent ends it and stays ready for the
    next prompt, with the conversation intact. */
let stop: t => unit;

/** Ends the conversation: the agent is stopped without a word and the
    next prompt starts a conversation with no memory of this one. */
let forget: t => unit;

/** What the agent last did to the project's files, and forgets it; if
    the agent did nothing since the last call, the cause is an edit by
    hand. */
let take_cause: t => Event.cause;
