(* The extractor: reads the .cmt files of a built dune project and draws
   them as a Portolan graph. Named types become nodes, top-level functions
   become edges, tests are attached to the edges they mention. *)

(* [extract ~root ~build_dir ~failing] reads every .cmt (and .cmti) under
   [root/build_dir/default] whose source file lives under [root].

   [failing] is the outcome of a test run: [None] if the tests were not
   run (every test is [Unknown]); [Some l] marks a test [Fail] when
   (its source file relative to [root], its first line) is in [l], and
   [Pass] otherwise.

   Modules that failed to type-check are skipped. [Error] only when no
   module could be read at all. *)
val extract :
  root:string ->
  build_dir:string ->
  failing:(string * int) list option ->
  (Portolan_graph.Graph.t, string) result
