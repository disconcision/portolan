/* Shim for the one thing CanvasView takes from Hazel's core: the type of
   a definition's id. In Portolan an id is the definition's qualified
   name (see CanvasGraph). */
module Id = {
  type t = string;
  let compare = String.compare;
};
