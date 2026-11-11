/* The extractor: reads the .cmt files of a built dune project and draws
   them as a Portolan graph. Named types become nodes, top-level functions
   become edges, tests are attached to the edges they exercise.

   Three kinds of test are known:

   - an inline test ([let%test "name" = ...], ppx_inline_test) and a
     function [let test_name () = ...]: attached to the functions its
     body mentions;
   - an Alcotest case, [Alcotest.test_case "name" `Quick f] or the same
     through a wrapper of one's own. Its name is "suite: name" when it is
     written inside the suite's list, as in [("suite", [case; case])],
     and "name" otherwise. Cases live in a file of their own, apart from
     what they test, so a case is attached to the project's functions
     outside its file that it reaches: those its body mentions, and those
     mentioned by the definitions of its own file that it mentions,
     however many deep. A case that reaches none is attached to the
     functions of its own file that it mentions. When the body is a
     function of the file ([let counts () = ...]), that function is the
     test: the test's handle is the function's, and it is not an edge. */

/* [extract ~root ~build_dir ~failing ~previous ~broken] reads every .cmt
   (and .cmti) under [root/build_dir/default] whose source file lives
   under [root].

   [failing] is the outcome of a test run: [None] if the tests were not
   run (every test is [Unknown]); [Some l] marks a test [Fail] when
   (its source file relative to [root], its first line) is in [l], and
   [Pass] otherwise. An Alcotest case fails when [l] has
   ("alcotest:" ++ its suite, its place in the suite, from 0), which is
   how Alcotest lists a failure ("[FAIL]  suite  0  name."), or a
   location anywhere inside it. A case outside a written-out suite cannot
   be named that way: it is [Unknown] when [l] names a case that is none
   of the known ones.

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
     added. Two things are found by where they were, not by name: a
     value that a ppx derived from a type follows that type, and an
     inline test without a description is the n-th such test of its
     module;
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
