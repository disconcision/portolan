/* Showing OCaml code in Reason syntax and the reverse, with `refmt`. */

/** The two syntaxes of OCaml: OCaml's own (`.ml`, `.mli`) and Reason
    (`.re`, `.rei`). */
type t =
  | Ml
  | Re;

/** ["ml"] and ["re"] name the syntaxes; anything else is [None]. */
let of_string: string => option(t);

/** [slice(~first, ~last, text)] is lines [first] to [last] of [text],
    counted from 1, both included; out-of-range bounds are clamped, and
    without [last] the slice runs to the end. */
let slice: (~first: int, ~last: option(int), string) => string;

/** [convert(~path, ~first, ~last, ~target)] is the slice of the source
    file at [path] in the syntax [target], and whether that worked. The
    file's own syntax is read off its extension; a slice already in the
    target syntax is returned as it is. When the slice cannot be converted
    (it does not parse on its own, the file is not OCaml or Reason, or
    `refmt` is missing) the result is the slice as written, with [false]. */
let convert:
  (~path: string, ~first: int, ~last: option(int), ~target: t) =>
  (string, bool);

/** [types(texts)] is each printed OCaml type (a type expression, or the
    right-hand side of a type definition) in Reason syntax, on one line;
    [None] for a text `refmt` cannot read. Answers are remembered. */
let types: list(string) => list(option(string));
