open Portolan_sidecar;
open Testing;

/* The patterns offered for a shell command nothing else suggests. */
let offered = command =>
  Permissions.suggest(
    ~tool="Bash",
    ~input=`Assoc([("command", `String(command))]),
    ~suggestions=`Null,
  );

let offers = (command, patterns) =>
  Alcotest.check(strings, command, patterns, offered(command));

let tests = [
  case("a command is widened to its leading word", () => {
    offers("python3 gen.py --all", ["Bash(python3:*)"]);
    offers("make", ["Bash(make:*)"]);
  }),
  case("a tool with sub-commands is widened to the sub-command", () => {
    offers("git push origin main", ["Bash(git push:*)"]);
    offers("git -C . status --short", ["Bash(git -C . status:*)"]);
    offers("opam install yojson", ["Bash(opam install:*)"]);
  }),
  case("a tool with sub-commands and none given stays exact", () =>
    offers("git --version", ["Bash(git --version)"])
  ),
  case("a risky command stays exact", () => {
    offers("rm -rf _build", ["Bash(rm -rf _build)"]);
    offers("sudo make install", ["Bash(sudo make install)"]);
    offers("curl https://example.com", ["Bash(curl https://example.com)"]);
  }),
  case("a command that is already a pattern is kept", () =>
    offers("make:*", ["Bash(make:*)"])
  ),
  case("a pipeline offers a pattern for each command, sorted", () =>
    offers(
      "ocamlformat --check lib/scene.ml && git -C . status --short | head -3",
      ["Bash(git -C . status:*)", "Bash(head:*)", "Bash(ocamlformat:*)"],
    )
  ),
  case("the same command twice is offered once", () =>
    offers("ls lib; ls test", ["Bash(ls:*)"])
  ),
  case("a file outside the project offers its folder", () =>
    Alcotest.check(
      strings,
      "patterns",
      ["Read(//etc/**)"],
      Permissions.suggest(
        ~tool="Read",
        ~input=parse({|{"file_path":"/etc/hosts"}|}),
        ~suggestions=`Null,
      ),
    )
  ),
  case("a tool with nothing to narrow it is offered whole", () => {
    let whole = (tool, input) =>
      Alcotest.check(
        strings,
        tool,
        [tool],
        Permissions.suggest(~tool, ~input=parse(input), ~suggestions=`Null),
      );
    whole("WebSearch", {|{"query":"ocaml"}|});
    whole("Edit", {|{"file_path":"lib/scene.ml"}|});
  }),
  case("Claude Code's suggested rules win, with commands widened", () =>
    Alcotest.check(
      strings,
      "patterns",
      ["Bash(git push:*)", "Bash(python3:*)", "Read(//etc/**)", "WebFetch"],
      Permissions.suggest(
        ~tool="Bash",
        ~input=parse({|{"command":"something else entirely"}|}),
        ~suggestions=
          parse(
            {|[{"type":"addRules","behavior":"allow","destination":"localSettings","rules":[
                 {"toolName":"Bash","ruleContent":"python3 gen.py"},
                 {"toolName":"Bash","ruleContent":"git push origin main"},
                 {"toolName":"Read","ruleContent":"//etc/**"},
                 {"toolName":"WebFetch"}]},
               {"type":"setMode","mode":"acceptEdits"},
               {"type":"addRules","behavior":"deny","rules":[{"toolName":"Bash","ruleContent":"ls"}]}]|},
          ),
      ),
    )
  ),
  case("a pattern is a tool and what it may do, as Claude Code's rule", () => {
    let rule = (pattern, expected) =>
      Alcotest.check(
        json,
        pattern,
        parse(expected),
        Permissions.rule(pattern),
      );
    rule("Bash(make:*)", {|{"toolName":"Bash","ruleContent":"make:*"}|});
    rule(
      "Bash(git -C . status)",
      {|{"toolName":"Bash","ruleContent":"git -C . status"}|},
    );
    rule("WebSearch", {|{"toolName":"WebSearch"}|});
  }),
  case("only patterns not yet remembered are added, trimmed, each once", () =>
    Alcotest.check(
      strings,
      "fresh",
      ["Bash(git commit:*)", "Read(//etc/**)"],
      Permissions.fresh(
        ~known=["Bash(git add:*)", "Bash(ocamlformat:*)"],
        [
          "Bash(git add:*)",
          " Bash(git commit:*) ",
          "Bash(ocamlformat:*)",
          "",
          "Read(//etc/**)",
          "Bash(git commit:*)",
        ],
      ),
    )
  ),
];
