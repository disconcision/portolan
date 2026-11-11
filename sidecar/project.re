let (/) = Filename.concat;

type t = {
  root: string,
  opam_switch: option(string),
  env: option(array(string)),
};

let root = p => p.root;
let name = p => Filename.basename(p.root);
let opam_switch = p => p.opam_switch;
let env = p => p.env;

let build_dir = "_build_portolan";
let ensure_build_dir = p => Files.private_dir(p.root / build_dir);

let state_dir = p => p.root / ".portolan";
let state = (p, name) => state_dir(p) / name;

let trace = p => {
  let path = state(p, "trace.jsonl");
  if (!Sys.file_exists(path)) {
    let stamped =
      Sys.readdir(state_dir(p))
      |> Array.to_list
      |> List.filter(f =>
           String.starts_with(~prefix="trace-", f)
           && String.ends_with(~suffix=".jsonl", f)
         )
      |> List.sort((a, b) => compare(b, a));
    switch (stamped) {
    | [newest, ..._] => Files.write(path, Files.read(state(p, newest)))
    | [] => ()
    };
  };
  path;
};

let is_file = path =>
  switch (Unix.stat(path).Unix.st_kind) {
  | Unix.S_REG => true
  | _
  | exception _ => false
  };

let file = (p, rel) =>
  if (rel != ""
      && Filename.is_relative(rel)
      && !List.mem("..", String.split_on_char('/', rel))
      && is_file(p.root / rel)) {
    Some(p.root / rel);
  } else {
    None;
  };

let relative = (p, path) => {
  let prefix = p.root ++ "/";
  let n = String.length(prefix);
  if (String.length(path) > n && String.starts_with(~prefix, path)) {
    Some(String.sub(path, n, String.length(path) - n));
  } else {
    None;
  };
};

let run = (~timeout=?, p, command) =>
  Run.cmd(~timeout?, ~env=?p.env, ~cwd=p.root, command);

/* ---------- the switch ---------- */

let is_dir = path =>
  try(Sys.is_directory(path)) {
  | Sys_error(_) => false
  };

/* "NAME=value" lines, as `env` prints them. A line that does not start a
   variable continues the value before it. */
let parse_env = out => {
  let starts_variable = line =>
    switch (String.index_opt(line, '=')) {
    | Some(i) when i > 0 =>
      String.for_all(
        c =>
          c == '_'
          || c >= 'A'
          && c <= 'Z'
          || c >= 'a'
          && c <= 'z'
          || c >= '0'
          && c <= '9',
        String.sub(line, 0, i),
      )
    | _ => false
    };
  List.fold_left(
    (acc, line) =>
      switch (acc) {
      | _ when starts_variable(line) => [line, ...acc]
      | [prev, ...rest] => [prev ++ "\n" ++ line, ...rest]
      | [] => []
      },
    [],
    String.split_on_char('\n', String.trim(out)),
  )
  |> List.rev
  |> Array.of_list;
};

let switch_env = name => {
  let enter =
    Printf.sprintf(
      "opam exec --switch=%s --set-switch -- env",
      Filename.quote(name),
    );
  let cwd = Sys.getcwd();
  switch (Run.cmd(~timeout=60., ~cwd, enter ++ " 2>/dev/null")) {
  | (0, out) when String.trim(out) != "" => Ok(parse_env(out))
  | _ =>
    let (_, why) = Run.cmd(~timeout=60., ~cwd, enter ++ " 2>&1 >/dev/null");
    Error(
      Printf.sprintf(
        "could not enter the opam switch %s\n%s",
        name,
        String.trim(why),
      ),
    );
  };
};

/* A switch given as a folder is named by its absolute path; the `_opam`
   folder inside one stands for the folder holding it. */
let switch_name = s =>
  if (is_dir(s)
      && (String.contains(s, '/') || String.starts_with(~prefix=".", s))) {
    let dir = Unix.realpath(s);
    Filename.basename(dir) == "_opam" ? Filename.dirname(dir) : dir;
  } else {
    s;
  };

let at = (~opam_switch=?, dir) => {
  let dir = Filename.is_relative(dir) ? Sys.getcwd() / dir : dir;
  if (!Sys.file_exists(dir)) {
    Files.mkdir_p(dir);
    Files.write(dir / "dune-workspace", "(lang dune 3.16)\n");
  };
  let root = Unix.realpath(dir);
  Files.private_dir(root / ".portolan");
  let entered =
    switch (opam_switch) {
    | Some(s) =>
      let s = switch_name(s);
      Result.map(env => (Some(s), Some(env)), switch_env(s));
    | None when is_dir(root / "_opam") =>
      /* Found, not asked for: if it cannot be entered, say so and carry on. */
      switch (switch_env(root)) {
      | Ok(env) => Ok((Some(root), Some(env)))
      | Error(message) =>
        prerr_endline("portolan: " ++ message);
        Ok((None, None));
      }
    | None => Ok((None, None))
    };
  Result.map(
    ((opam_switch, env)) =>
      {
        root,
        opam_switch,
        env,
      },
    entered,
  );
};
