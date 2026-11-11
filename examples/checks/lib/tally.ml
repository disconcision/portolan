(* Tallies: how many times each name was seen. The Alcotest cases that
   exercise this file are in ../test. *)

(* [equal_entry] and [equal] are written by ppx_deriving: they are
   nowhere in this text. *)
type entry = string * int [@@deriving eq]

type t = entry list [@@deriving eq]

type report = {
  total : int;
  top : entry option;
  by_name : t;
}

(* another name for a type from outside *)
type out = Buffer.t

let empty : t = []

let add (name : string) (tally : t) : t =
  match List.assoc_opt name tally with
  | Some n -> (name, n + 1) :: List.remove_assoc name tally
  | None -> (name, 1) :: tally

let count (name : string) (tally : t) : int =
  Option.value (List.assoc_opt name tally) ~default:0

let total (tally : t) : int = List.fold_left (fun acc (_, n) -> acc + n) 0 tally

let%test _ = total empty = 0

let top (tally : t) : entry option =
  List.fold_left
    (fun best (name, n) ->
      match best with
      | Some (_, m) when m >= n -> best
      | _ -> Some (name, n))
    None tally

let report (tally : t) : report = { total = total tally; top = top tally; by_name = tally }

(* a function as an argument, and a type applied to two others *)
let pick (keep : entry -> bool) (tally : t) : (t, string) result =
  match List.filter keep tally with
  | [] -> Error "nothing kept"
  | kept -> Ok kept

let print (out : out) (tally : t) : unit =
  List.iter (fun (name, n) -> Buffer.add_string out (Printf.sprintf "%s %d\n" name n)) tally

let%test _ = count "a" (add "a" empty) = 1

(* The signature runs over several lines before the body starts. *)
module Scale : sig
  val double : t -> t
  val halve : t -> t
end = struct
  let double (tally : t) : t = List.map (fun (name, n) -> (name, 2 * n)) tally
  let halve (tally : t) : t = List.map (fun (name, n) -> (name, n / 2)) tally
end
