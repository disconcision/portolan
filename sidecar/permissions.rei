/* What the agent may do without asking, beyond the built-in list: the
   patterns the user chose "always allow" for in this project. A pattern
   is a Claude Code permission rule, such as `Bash(make:*)` (any command
   starting with `make`), `Bash(git -C . status)` (that command) or
   `WebSearch` (the whole tool). They are kept one per line in
   `.portolan/allow`, which can be edited by hand. */

/** The patterns remembered for a project, oldest first. */
let remembered: Project.t => list(string);

/** Adds patterns to those remembered for a project; ones already there
    are not repeated. */
let remember: (Project.t, list(string)) => unit;

/** The patterns to offer for "always allow" when the agent asks to use
    [tool] with [input]. [suggestions] are the rules Claude Code proposes
    with the request (its `permission_suggestions`), which name the exact
    commands; a command is widened to its leading word, as in
    `Bash(python3:*)`, or to its sub-command for tools such as git and
    opam, as in `Bash(git push:*)`. Commands that are risky as a family
    (`rm`, `sudo`, `curl`, a shell) stay exact. The user sees the patterns
    and can change them before agreeing. */
let suggest:
  (~tool: string, ~input: Yojson.Safe.t, ~suggestions: Yojson.Safe.t) =>
  list(string);

/** A pattern as the rule Claude Code takes in a permission update:
    `{toolName, ruleContent}`. */
let rule: string => Yojson.Safe.t;
