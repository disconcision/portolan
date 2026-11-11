(** Lengths and vectors. *)

(** A length in millimetres. Its representation is hidden. *)
type length

type vec = { x : int; y : int }

val mm : int -> length
val to_mm : length -> int

(** Component-wise sum. *)
val add : vec -> vec -> vec

val norm1 : vec -> length

module Angle : sig
  type t = Deg of int | Turn

  val to_deg : t -> int
  val flip : t -> t
end
