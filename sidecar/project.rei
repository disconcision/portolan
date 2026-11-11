/* The watched project: where it is, the folders Portolan keeps in it, and
   the opam switch its commands run in. */

/** A dune project on disk. */
type t;

/** [at(dir)] is the project in the folder [dir]. A folder that does not
    exist is created, with a `dune-workspace` so dune treats it as its own
    root. The state folder is created, ignored by the project's git.

    [opam_switch] names the opam switch (by name, or by the folder of a local
    switch) whose environment the project's commands and the agent run
    in. Without it, a local `_opam` folder in the project is used if there
    is one; otherwise the sidecar's own environment. [Error] with a
    message if the named switch cannot be entered. */
let at: (~opam_switch: string=?, string) => result(t, string);

/** The project's absolute path, symbolic links resolved. */
let root: t => string;

/** The project folder's name, for display. */
let name: t => string;

/** The opam switch the project's commands run in, if not the sidecar's. */
let opam_switch: t => option(string);

/** The environment of that switch, for starting processes in it; [None]
    means the sidecar's own environment. */
let env: t => option(array(string));

/** [state(p, name)] is the path of the file [name] in the project's
    `.portolan` folder, where the sidecar keeps the trace, the last graph
    and the agent's error output. */
let state: (t, string) => string;

/** The continuing trace of the project, `.portolan/trace.jsonl`. If only
    traces named by start time exist (`trace-<stamp>.jsonl`, written by
    earlier versions), the newest is copied to start it. */
let trace: t => string;

/** The name of the build folder Portolan builds the project into, kept
    apart from the project's own `_build`. */
let build_dir: string;

/** Makes sure that build folder exists and is ignored by the project's
    git. */
let ensure_build_dir: t => unit;

/** [file(p, rel)] is the absolute path of the project file at the
    relative path [rel], if it is an existing file and [rel] does not step
    outside the project. */
let file: (t, string) => option(string);

/** [relative(p, path)] is an absolute [path] under the project made
    relative to its root; [None] for a path outside the project. */
let relative: (t, string) => option(string);

/** Runs a shell command in the project's root and switch environment.
    Returns the exit code and output, as [Run.cmd] does. */
let run: (~timeout: float=?, t, string) => (int, string);
