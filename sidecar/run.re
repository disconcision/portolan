let cmd = (~timeout=120., ~env=?, ~cwd, command) => {
  let (r, w) = Unix.pipe(~cloexec=true, ());
  let argv = [|
    "/bin/sh",
    "-c",
    Printf.sprintf("cd %s && %s", Filename.quote(cwd), command),
  |];
  let devnull = Unix.openfile("/dev/null", [Unix.O_RDONLY], 0);
  let pid =
    switch (env) {
    | Some(env) =>
      Unix.create_process_env("/bin/sh", argv, env, devnull, w, w)
    | None => Unix.create_process("/bin/sh", argv, devnull, w, w)
    };
  Unix.close(devnull);
  Unix.close(w);
  let buf = Buffer.create(4096)
  and chunk = Bytes.create(4096);
  let deadline = Unix.gettimeofday() +. timeout;
  let timed_out = ref(false);
  let rec loop = () => {
    let left = deadline -. Unix.gettimeofday();
    if (left <= 0.) {
      timed_out := true;
      try(Unix.kill(pid, Sys.sigkill)) {
      | Unix.Unix_error(_) => ()
      };
    } else {
      switch (Unix.select([r], [], [], Float.min(left, 1.0))) {
      | ([], _, _) => loop()
      | _ =>
        let n =
          try(Unix.read(r, chunk, 0, 4096)) {
          | Unix.Unix_error(_) => 0
          };
        if (n > 0) {
          Buffer.add_subbytes(buf, chunk, 0, n);
          loop();
        };
      | exception (Unix.Unix_error(Unix.EINTR, _, _)) => loop()
      };
    };
  };
  loop();
  Unix.close(r);
  let code =
    switch (snd(Unix.waitpid([], pid))) {
    | Unix.WEXITED(n) => n
    | _ => 125
    | exception (Unix.Unix_error(_)) => 125
    };
  (timed_out^ ? 124 : code, Buffer.contents(buf));
};
