/* Shim for Hazel's src/util/JsUtil.re: only the functions the canvas
   files call, with upstream's bodies. */
open Js_of_ocaml;

let get_elem_by_id = id => {
  let doc = Dom_html.document;
  Js.Opt.get(doc##getElementById(Js.string(id)), () => {assert(false)});
};

let ids_with_prefix = (prefix: string): list(string) =>
  /* headless (node tests): no document, so no elements to animate */
  switch (
    Dom_html.document##querySelectorAll(
      Js.string("[id^=\"" ++ prefix ++ "\"]"),
    )
  ) {
  | exception _ => []
  | nodes =>
    List.init(nodes##.length, i => nodes##item(i))
    |> List.filter_map(n =>
         Js.Opt.case(
           n,
           () => None,
           n => Some(Js.to_string(Js.Unsafe.get(n, "id"))),
         )
       )
  };

let get_elem_by_id_opt = id =>
  switch (get_elem_by_id(id)) {
  | exception _ => None
  | e => Some(e)
  };

/* upstream caches this in a one-element slot cleared on resize */
let cached_client_height = (el: Js.t(Dom_html.element)): float =>
  float_of_int(el##.clientHeight);
