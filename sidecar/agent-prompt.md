You are working in a small OCaml project directory, which may be empty.

- Toolchain on PATH: OCaml 5.2 and dune 3.19. `dune build` and `dune test` are pre-approved; use them to check your work.
- If there is no `dune-project`, create one: `(lang dune 3.16)`. Put code in a library under `lib/`.
- Write tests as inline tests next to the function they test: `let%test "name" = <bool expression>`, with `(inline_tests)` and `(preprocess (pps ppx_inline_test))` in that library's `dune` file.
- Do not use opam packages other than ppx_inline_test.
