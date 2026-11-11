(* Alcotest cases for Tally and Window, written the ways they usually are. *)

open Checks

let seen (names : string list) : Tally.t = List.fold_left (fun t n -> Tally.add n t) Tally.empty names

let abc = seen [ "a"; "b"; "a"; "c"; "a" ]

(* the body is a function of this file *)
let counts () = Alcotest.(check int) "a three times" 3 (Tally.count "a" abc)

(* reaches Tally.add only through [seen] *)
let adds () = Alcotest.(check int) "one name" 1 (List.length (seen [ "a"; "a" ]))

let top () =
  Alcotest.(check (option (pair string int))) "a leads" (Some ("a", 3)) (Tally.top abc)

(* mentions nothing outside this file *)
let twice (n : int) : int = 2 * n
let doubles () = Alcotest.(check int) "two twos" 4 (twice 2)

(* a wrapper of one's own *)
let quick name f = Alcotest.test_case name `Quick f

(* cases outside the suite they are run in *)
let window_cases =
  [
    Alcotest.test_case "inside" `Quick (fun () ->
        Alcotest.(check int) "two entries within 1..2" 2
          (List.length (Window.inside { low = 1; high = 2 } abc)));
  ]

let () =
  Alcotest.run "checks"
    [
      ( "tally",
        [
          Alcotest.test_case "counts" `Quick counts;
          quick "adds" adds;
          quick "top" top;
          Alcotest.test_case "total" `Quick (fun () ->
              Alcotest.(check int) "five in all" 5 (Tally.total abc));
          quick "doubles" doubles;
        ] );
      ("window", window_cases);
    ]
