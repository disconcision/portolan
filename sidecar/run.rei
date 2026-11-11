/* Running shell commands. */

/** [cmd(~cwd, command)] runs [command] with `/bin/sh` in the directory
    [cwd] and returns its exit code and its output. The command's standard
    output is captured; add `2>&1` to capture its errors too. With [env]
    the command sees that environment instead of the sidecar's own. A
    command still running after [timeout] seconds (120 unless given) is
    killed and reported with exit code 124. */
let cmd:
  (~timeout: float=?, ~env: array(string)=?, ~cwd: string, string) =>
  (int, string);
