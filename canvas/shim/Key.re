/* Shim for Hazel's src/util/Key.re. */
open Js_of_ocaml;

let get_key = evt =>
  Js.to_string(Js.Optdef.get(evt##.key, () => failwith("JsUtil.get_key")));
