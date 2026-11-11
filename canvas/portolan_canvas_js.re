/* The bundle's entry point: `window.PortolanCanvas`.

     PortolanCanvas.mount(element)
     PortolanCanvas.push(snapshot, cause)   snapshot: Graph JSON (string or
                                            object); cause: {label, site?,
                                            ok?} (string or object)
     PortolanCanvas.show(snapshot)          no story: shown as it is
     PortolanCanvas.reset()
     PortolanCanvas.setFollow(bool)         the camera follows the avatar
     PortolanCanvas.setPace(bool)           false: snapshots jump-cut
     PortolanCanvas.setBusy(bool)           the agent is working
     PortolanCanvas.setThought(text)        streamed reasoning (thought cloud)
     PortolanCanvas.fit()
     PortolanCanvas.onSelect(fn(name, kind, at))
     PortolanCanvas.pending()               beats still queued
     PortolanCanvas.log()                   the canvas journal, as text
     PortolanCanvas.layouts()               [{id, label}]: the layouts on offer
     PortolanCanvas.edgeStyles()            [{id, label}]: the edge styles
     PortolanCanvas.layout() / edges()      the ones in use (ids)
     PortolanCanvas.setLayout(id)           lays the board out again; false
     PortolanCanvas.setEdges(id)            for an id that is not on offer
     PortolanCanvas.contours()              [{id, label}]: what a module's
                                            region is drawn around
     PortolanCanvas.pulls()                 [{id, label}]: how hard a module's
                                            members are drawn together
     PortolanCanvas.contour() / pull()      the ones in use (ids)
     PortolanCanvas.setContour(id)          false for an id that is not on
     PortolanCanvas.setPull(id)             offer
     PortolanCanvas.layoutStats()           {calls, ms, hits, phases}: layouts
                                            computed, their total time, memo
                                            hits, time by phase */
open Js_of_ocaml;
open Portolan_canvas;

let json_of_any = (v: Js.Unsafe.any): Yojson.Safe.t => {
  let s: string =
    Js.to_string(Js.typeof(v)) == "string"
      ? Js.to_string(Js.Unsafe.coerce(v))
      : Js.to_string(Js._JSON##stringify(v));
  Yojson.Safe.from_string(s);
};

/* a snapshot that is not a graph record is an error [guard] reports */
let graph_of_any = (v: Js.Unsafe.any): CanvasGraph.t =>
  switch (CanvasGraph.of_json(json_of_any(v))) {
  | Ok(g) => g
  | Error(message) => failwith("not a graph record: " ++ message)
  };

/* neither undefined nor null */
let present = (v: Js.Unsafe.any): bool => {
  let ty = Js.to_string(Js.typeof(Js.Unsafe.coerce(v)));
  ty == "string" || ty == "object" && Js.Opt.test(Obj.magic(v));
};

let cause_of_json = (j: Yojson.Safe.t): CanvasDirector.cause => {
  let mem = k =>
    switch (j) {
    | `Assoc(kvs) => Option.value(List.assoc_opt(k, kvs), ~default=`Null)
    | _ => `Null
    };
  {
    label:
      switch (mem("label")) {
      | `String(s) => s
      | _ => "edit"
      },
    site:
      switch (mem("site")) {
      | `String(s) when s != "" => Some(s)
      | _ => None
      },
    ok:
      switch (mem("ok"), mem("err"), mem("status")) {
      | (`Bool(b), _, _) => b
      | (_, `Bool(b), _) => !b
      | (_, `String(_), _) => false
      | (_, _, `String(s)) => s != "err" && s != "error"
      | _ => true
      },
  };
};

let guard = (what: string, f: unit => unit): unit =>
  try(f()) {
  | exn =>
    let msg = "PortolanCanvas." ++ what ++ ": " ++ Printexc.to_string(exn);
    CanvasLog.log(msg);
    ignore(
      Js.Unsafe.meth_call(
        Js.Unsafe.get(Js.Unsafe.global, "console"),
        "error",
        [|Js.Unsafe.inject(Js.string(msg))|],
      ),
    );
  };

/* [{id, label}, ...] */
let options = (l: list((string, string))) =>
  Js.array(
    Array.of_list(
      List.map(
        ((id, label)) =>
          Js.Unsafe.obj([|
            ("id", Js.Unsafe.inject(Js.string(id))),
            ("label", Js.Unsafe.inject(Js.string(label))),
          |]),
        l,
      ),
    ),
  );

let () = {
  let fn1 = (f: Js.Unsafe.any => unit) =>
    Js.Unsafe.inject(Js.wrap_callback(f));
  let api =
    Js.Unsafe.obj([|
      (
        "mount",
        fn1(el =>
          guard("mount", () => CanvasDirector.mount(Js.Unsafe.coerce(el)))
        ),
      ),
      (
        "push",
        Js.Unsafe.inject(
          /* exact arity: push(snapshot) alone must still run */
          Js.Unsafe.callback_with_arity(
            2, (snap: Js.Unsafe.any, cause: Js.Unsafe.any) =>
            guard("push", () => {
              let c =
                present(cause)
                  ? cause_of_json(json_of_any(cause))
                  : {
                    label: "edit",
                    site: None,
                    ok: true,
                  };
              CanvasDirector.push(graph_of_any(snap), c);
            })
          ),
        ),
      ),
      (
        "show",
        fn1(snap =>
          guard("show", () => CanvasDirector.show(graph_of_any(snap)))
        ),
      ),
      ("reset", fn1(_ => guard("reset", CanvasDirector.reset))),
      (
        "setFollow",
        fn1(b =>
          CanvasDirector.set_follow(Js.to_bool(Js.Unsafe.coerce(b)))
        ),
      ),
      (
        "setPace",
        fn1(b => CanvasDirector.set_pace(Js.to_bool(Js.Unsafe.coerce(b)))),
      ),
      (
        "setBusy",
        fn1(b => CanvasDirector.set_busy(Js.to_bool(Js.Unsafe.coerce(b)))),
      ),
      (
        "setThought",
        fn1(s =>
          CanvasDirector.set_thought(Js.to_string(Js.Unsafe.coerce(s)))
        ),
      ),
      ("fit", fn1(_ => guard("fit", CanvasDirector.fit))),
      (
        "onSelect",
        fn1(cb =>
          CanvasDirector.on_select :=
            (
              (name, kind, at) => {
                let at =
                  switch (at) {
                  | Some((h: CanvasGraph.handle)) =>
                    Js.Unsafe.obj([|
                      ("file", Js.Unsafe.inject(Js.string(h.file))),
                      ("path", Js.Unsafe.inject(Js.string(h.path))),
                      ("line", Js.Unsafe.inject(h.line)),
                      ("end_line", Js.Unsafe.inject(h.end_line)),
                    |])
                  | None => Js.Unsafe.inject(Js.null)
                  };
                ignore(
                  Js.Unsafe.fun_call(
                    cb,
                    [|
                      Js.Unsafe.inject(Js.string(name)),
                      Js.Unsafe.inject(Js.string(kind)),
                      Js.Unsafe.inject(at),
                    |],
                  ),
                );
              }
            )
        ),
      ),
      (
        "pending",
        Js.Unsafe.inject(Js.wrap_callback(() => CanvasDirector.pending())),
      ),
      (
        "layouts",
        Js.Unsafe.inject(
          Js.wrap_callback(() => options(CanvasOptions.layouts)),
        ),
      ),
      (
        "edgeStyles",
        Js.Unsafe.inject(
          Js.wrap_callback(() => options(CanvasOptions.edge_styles)),
        ),
      ),
      (
        "layout",
        Js.Unsafe.inject(
          Js.wrap_callback(() => Js.string(CanvasOptions.layout())),
        ),
      ),
      (
        "edges",
        Js.Unsafe.inject(
          Js.wrap_callback(() => Js.string(CanvasOptions.edges())),
        ),
      ),
      (
        "setLayout",
        Js.Unsafe.inject(
          Js.wrap_callback((k: Js.Unsafe.any) =>
            Js.bool(
              CanvasDirector.set_layout(Js.to_string(Js.Unsafe.coerce(k))),
            )
          ),
        ),
      ),
      (
        "select",
        Js.Unsafe.inject(
          Js.wrap_callback((name: Js.Unsafe.any, is_type: Js.Unsafe.any) =>
            guard("select", () =>
              CanvasDirector.select(
                ~is_type=Js.to_bool(Js.Unsafe.coerce(is_type)),
                Js.to_string(Js.Unsafe.coerce(name)),
              )
            )
          ),
        ),
      ),
      (
        "setEdges",
        Js.Unsafe.inject(
          Js.wrap_callback((k: Js.Unsafe.any) =>
            Js.bool(
              CanvasDirector.set_edges(Js.to_string(Js.Unsafe.coerce(k))),
            )
          ),
        ),
      ),
      (
        "contours",
        Js.Unsafe.inject(
          Js.wrap_callback(() => options(CanvasContour.styles)),
        ),
      ),
      (
        "pulls",
        Js.Unsafe.inject(
          Js.wrap_callback(() => options(CanvasPull.strengths)),
        ),
      ),
      (
        "contour",
        Js.Unsafe.inject(
          Js.wrap_callback(() => Js.string(CanvasContour.chosen())),
        ),
      ),
      (
        "pull",
        Js.Unsafe.inject(
          Js.wrap_callback(() => Js.string(CanvasPull.id())),
        ),
      ),
      (
        "setContour",
        Js.Unsafe.inject(
          Js.wrap_callback((k: Js.Unsafe.any) =>
            Js.bool(
              CanvasDirector.set_contour(Js.to_string(Js.Unsafe.coerce(k))),
            )
          ),
        ),
      ),
      (
        "setPull",
        Js.Unsafe.inject(
          Js.wrap_callback((k: Js.Unsafe.any) =>
            Js.bool(
              CanvasDirector.set_pull(Js.to_string(Js.Unsafe.coerce(k))),
            )
          ),
        ),
      ),
      (
        "layoutStats",
        Js.Unsafe.inject(
          Js.wrap_callback(() =>
            Js.Unsafe.obj([|
              ("calls", Js.Unsafe.inject(CanvasLayout.layout_calls^)),
              ("ms", Js.Unsafe.inject(CanvasLayout.layout_ms^)),
              ("hits", Js.Unsafe.inject(CanvasLayout.layout_memo_hits^)),
              (
                "phases",
                Js.Unsafe.inject(Js.string(Util.PerfTimer.summary())),
              ),
            |])
          ),
        ),
      ),
      (
        "log",
        Js.Unsafe.inject(
          Js.wrap_callback(() =>
            Js.string(String.concat("\n", List.rev(CanvasLog.entries^)))
          ),
        ),
      ),
    |]);
  Js.Unsafe.set(Js.Unsafe.global, "PortolanCanvas", api);
};
