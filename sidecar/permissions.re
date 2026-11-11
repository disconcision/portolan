let file = project => Project.state(project, "allow");

let remembered = project =>
  try(List.map(String.trim, Files.lines(Files.read(file(project))))) {
  | Sys_error(_) => []
  };

let fresh = (~known, patterns) =>
  List.rev(
    List.fold_left(
      (acc, p) => {
        let p = String.trim(p);
        p == "" || List.mem(p, known) || List.mem(p, acc)
          ? acc : [p, ...acc];
      },
      [],
      patterns,
    ),
  );

let remember = (project, patterns) => {
  let known = remembered(project);
  let fresh = fresh(~known, patterns);
  if (fresh != []) {
    Files.write(file(project), String.concat("\n", known @ fresh) ++ "\n");
  };
};

/* ---------- patterns to offer ---------- */

/* Tools whose first argument names what they do. */
let families = [
  "git",
  "opam",
  "dune",
  "esy",
  "npm",
  "npx",
  "yarn",
  "pnpm",
  "cargo",
  "docker",
  "gh",
  "brew",
  "pip",
  "pip3",
];

let risky = [
  "rm",
  "sudo",
  "sh",
  "bash",
  "zsh",
  "eval",
  "curl",
  "wget",
  "chmod",
  "chown",
  "kill",
  "dd",
];

let is_word = s =>
  s != ""
  && s.[0] >= 'a'
  && s.[0] <= 'z'
  && String.for_all(
       c => c >= 'a' && c <= 'z' || c >= '0' && c <= '9' || c == '-',
       s,
     );

/* `git -C . status --short` -> `git -C . status`: up to the sub-command. */
let rec through_subcommand =
  fun
  | [] => None
  | [w, ..._] when is_word(w) => Some([w])
  | [w, ...rest] =>
    Option.map(found => [w, ...found], through_subcommand(rest));

let widen = command =>
  switch (List.filter(w => w != "", String.split_on_char(' ', command))) {
  /* Already a pattern. */
  | _
      when
        String.ends_with(~suffix=":*", command)
        || String.ends_with(~suffix=" *", command) => command
  | [first, ...rest] when List.mem(first, families) =>
    switch (through_subcommand(rest)) {
    | Some(words) => String.concat(" ", [first, ...words]) ++ ":*"
    | None => command
    }
  | [first, ..._] when is_word(first) && !List.mem(first, risky) =>
    first ++ ":*"
  | _ => command
  };

/* The commands of a pipeline or a sequence, split where the shell would;
   quoting is not looked at, which is good enough for a suggestion. */
let parts = command =>
  Str.split(Str.regexp("&&\\|||\\|[|;\n]"), command)
  |> List.map(String.trim)
  |> List.filter(part => part != "");

let suggest = (~tool, ~input, ~suggestions) => {
  open Yojson.Safe.Util;
  let text = (json, name) =>
    switch (member(name, json)) {
    | `String(s) => Some(s)
    | _ => None
    };
  let rules =
    (
      try(to_list(suggestions)) {
      | _ => []
      }
    )
    |> List.filter(s =>
         text(s, "type") == Some("addRules")
         && text(s, "behavior") == Some("allow")
       )
    |> List.concat_map(s =>
         try(to_list(member("rules", s))) {
         | _ => []
         }
       )
    |> List.filter_map(r =>
         switch (text(r, "toolName"), text(r, "ruleContent")) {
         | (Some("Bash"), Some(command)) =>
           Some("Bash(" ++ widen(command) ++ ")")
         | (Some(name), Some(content)) =>
           Some(name ++ "(" ++ content ++ ")")
         | (Some(name), None) => Some(name)
         | (None, _) => None
         }
       );
  let fallback =
    switch (tool, text(input, "command"), text(input, "file_path")) {
    | ("Bash", Some(command), _) =>
      List.map(part => "Bash(" ++ widen(part) ++ ")", parts(command))
    /* A file outside the project: everything in its folder. */
    | (_, _, Some(path)) when !Filename.is_relative(path) => [
        tool ++ "(/" ++ Filename.dirname(path) ++ "/**)",
      ]
    | _ => [tool]
    };
  List.sort_uniq(compare, rules == [] ? fallback : rules);
};

let rule = pattern =>
  switch (String.index_opt(pattern, '(')) {
  | Some(i) when String.ends_with(~suffix=")", pattern) =>
    `Assoc([
      ("toolName", `String(String.sub(pattern, 0, i))),
      (
        "ruleContent",
        `String(String.sub(pattern, i + 1, String.length(pattern) - i - 2)),
      ),
    ])
  | _ => `Assoc([("toolName", `String(pattern))])
  };
