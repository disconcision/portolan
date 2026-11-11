/* The extractor: reads the .cmt files of a built dune project and draws
   them as a Portolan graph. Named types become nodes, top-level functions
   become edges, tests are attached to the edges they mention. */

/* [extract ~root ~build_dir ~failing ~previous ~broken] reads every .cmt
   (and .cmti) under [root/build_dir/default] whose source file lives
   under [root].

   [failing] is the outcome of a test run: [None] if the tests were not
   run (every test is [Unknown]); [Some l] marks a test [Fail] when
   (its source file relative to [root], its first line) is in [l], and
   [Pass] otherwise.

   Modules that failed to type-check have no usable annotations. With
   [~previous:None] they are skipped, and the result is [Error] when no
   module could be read at all.

   [previous] is the graph of an earlier extraction of the same project.
   Every file it has definitions in that cannot be read this time, and
   still exists, is carried over from it: its nodes, edges, values and
   tests as they were, except that

   - each handle's [line] and [end_line] are where the item is in the
     source now (see Locate: from a parse tree when the file parses, from
     a scan of the text when it does not, unchanged when neither finds
     it). When the file parses, a definition whose name is gone from it
     is dropped. A definition the earlier graph does not have is not
     added;
   - [n_err], [e_err] and [v_err] are reset, then set as [broken] says;
   - its tests are [Unknown];
   - [main] and the deps stay as they were, less the names that no
     longer exist.

   "Cannot be read" also covers annotations older than the source (dune
   leaves those in place when a file stops parsing) and a module whose
   interface did not compile (what it exports is then unknown). Such
   annotations are only set aside for a file the earlier graph has.

   The other modules are extracted as usual. Keys stay consistent between
   the two: a type of a carried module keeps its node wherever it is
   referred to, and a type a carried definition refers to that no longer
   exists anywhere becomes a [Ghost] node under the same key. There are
   no duplicate keys and no [src], [dst] or [parts] without a node.

   [broken] are the compiler's error locations (file relative to [root],
   line). A definition, carried or not, whose span holds one is marked
   [err]; an error in an interface (.mli, .rei) marks the definition its
   [val] or [type] stands for. */
let extract:
  (
    ~root: string,
    ~build_dir: string,
    ~failing: option(list((string, int))),
    ~previous: option(Portolan_graph.Graph.t),
    ~broken: list((string, int))
  ) =>
  result(Portolan_graph.Graph.t, string);
