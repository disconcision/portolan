/* Http: which file a path on the notebook's side of the server names. */

open Portolan_sidecar;
open Testing;

let file = Alcotest.(option(string));
let site_file = Http.site_file(~home="/p");
let study = "/studies/2026-11-10-colour-schemes/";

let tests = [
  case("a study folder shows its index page", () =>
    Alcotest.check(
      file,
      "index",
      Some("/p/site/studies/2026-11-10-colour-schemes/index.html"),
      site_file(study),
    )
  ),
  case("a study's canvas bundle is the built one", () =>
    Alcotest.check(
      file,
      "bundle",
      Some("/p/canvas/dist/portolan_canvas.js"),
      site_file(study ++ "lib/portolan_canvas.js"),
    )
  ),
  case("a study's schemes are the page's", () =>
    Alcotest.check(
      file,
      "schemes",
      Some("/p/web/schemes.js"),
      site_file(study ++ "lib/schemes.js"),
    )
  ),
  case("the rest of a study's lib is its own", () =>
    Alcotest.check(
      file,
      "specimens",
      Some("/p/site/studies/2026-11-10-colour-schemes/lib/specimens.js"),
      site_file(study ++ "lib/specimens.js"),
    )
  ),
  case("the notebook's shared files are served", () =>
    Alcotest.check(
      file,
      "assets",
      Some("/p/site/assets/notebook.css"),
      site_file("/assets/notebook.css"),
    )
  ),
  case("the page's own files are not the notebook's", () =>
    Alcotest.check(file, "page", None, site_file("/style.css"))
  ),
];
