type length = int
type vec = { x : int; y : int }

(* private: not in the interface *)
let clamp (n : int) : int = if n < 0 then 0 else n
let mm (n : int) : length = clamp n
let to_mm (l : length) : int = l
let add (a : vec) (b : vec) : vec = { x = a.x + b.x; y = a.y + b.y }
let norm1 (v : vec) : length = mm (abs v.x + abs v.y)

module Angle = struct
  type t = Deg of int | Turn

  (* private: not in the interface *)
  let normalise (d : int) : int = ((d mod 360) + 360) mod 360

  let to_deg (a : t) : int =
    match a with
    | Deg d -> normalise d
    | Turn -> 0

  let flip (a : t) : t = Deg (to_deg a + 180)
end
