open Portolan_sidecar;
open Testing;

let tests = [
  case("Claude Code is started in the project, asking on its stream", () =>
    Alcotest.(check(string))(
      "command",
      "cd '/work/scene' && exec claude -p --input-format stream-json --output-format stream-json --verbose --include-partial-messages --permission-mode acceptEdits --permission-prompt-tool stdio --allowedTools 'Bash(dune:*)' 'WebSearch'",
      Claude.command(
        ~root="/work/scene",
        ~resume=None,
        ~model=None,
        ~instructions="",
        ~allow=["Bash(dune:*)", "WebSearch"],
      ),
    )
  ),
  case("a session, a model and instructions are passed on, quoted", () =>
    Alcotest.(check(string))(
      "command",
      "cd '/work/it'\\''s here' && exec claude -p --input-format stream-json --output-format stream-json --verbose --include-partial-messages --permission-mode acceptEdits --permission-prompt-tool stdio --resume 'abc-123' --model 'haiku' --append-system-prompt 'Be brief.\nDon'\\''t guess.' --allowedTools",
      Claude.command(
        ~root="/work/it's here",
        ~resume=Some("abc-123"),
        ~model=Some("haiku"),
        ~instructions="Be brief.\nDon't guess.",
        ~allow=[],
      ),
    )
  ),
  case("every dune command and the read-only helpers need no asking", () => {
    let allowed = pattern => List.mem(pattern, Claude.default_allow);
    Alcotest.(check(bool))(
      "allowed",
      true,
      allowed("Bash(dune:*)")
      && allowed("Bash(git status:*)")
      && allowed("Bash(cat:*)"),
    );
    Alcotest.(check(bool))(
      "not allowed",
      false,
      allowed("Bash(git:*)") || allowed("Bash(rm:*)"),
    );
  }),
];
