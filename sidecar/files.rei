/* Small file helpers shared by the sidecar's modules. */

/** The whole contents of the file at a path. Raises [Sys_error] if it
    cannot be read. */
let read: string => string;

/** [write(path, text)] replaces the file at [path] with [text]. */
let write: (string, string) => unit;

/** Creates a directory and any missing parents. */
let mkdir_p: string => unit;

/** Creates a directory that the enclosing git repository ignores without
    its own `.gitignore` being edited: the directory gets a `.gitignore`
    holding `*`. Safe to call again; an existing `.gitignore` is kept. */
let private_dir: string => unit;

/** [lines(text)] is the non-blank lines of [text]. */
let lines: string => list(string);
