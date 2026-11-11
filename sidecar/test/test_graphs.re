open Portolan_sidecar;
open Testing;

/* A graph of [n] nodes; the sidecar never looks inside one. */
let graph = n =>
  `Assoc([
    (
      "nodes",
      `List(
        List.init(n, i =>
          `Assoc([
            ("key", `String(Printf.sprintf("Scene.node_%d", i))),
            ("ty", `String("int -> int")),
          ])
        ),
      ),
    ),
    ("edges", `List([])),
  ]);

let size = json => String.length(Yojson.Safe.to_string(json));

let graph_line = (time, drawing) =>
  Event.Line.make(
    ~time,
    Graph({
      drawing,
      cause: By_hand,
      partial: false,
    }),
  );

let build = time =>
  Event.Line.make(
    ~time,
    Build({
      ok: true,
      output: "",
      ms: 5,
    }),
  );

let texts = lines => List.map(Event.Line.to_string, lines);

/* The graph a viewer arriving after [lines] is sent. */
let latest = lines =>
  List.find_map(Event.Line.drawing, Graphs.collapse(lines));

let tests = [
  case("the first graph is sent whole", () =>
    Alcotest.(check(bool))(
      "whole",
      true,
      Graphs.encode(~previous=None, ~since=0, graph(20))
      == (Whole(graph(20)), 0),
    )
  ),
  case("a small change is sent as a change, and counted", () =>
    switch (
      Graphs.encode(~previous=Some(graph(20)), ~since=100, graph(21))
    ) {
    | (Change(change), since) =>
      Alcotest.check(
        json,
        "the change gives the new graph",
        graph(21),
        Delta.apply(graph(20), change),
      );
      Alcotest.(check(int))("bytes since", 100 + size(change), since);
    | (Whole(_), _) => Alcotest.fail("sent whole")
    }
  ),
  case("a change half the size of the graph is sent whole", () =>
    Alcotest.(check(bool))(
      "whole",
      true,
      Graphs.encode(~previous=Some(graph(2)), ~since=0, graph(20))
      == (Whole(graph(20)), 0),
    )
  ),
  case("changes that add up to a whole graph send it whole again", () => {
    let whole = size(graph(21));
    Alcotest.(check(bool))(
      "whole",
      true,
      Graphs.encode(
        ~previous=Some(graph(20)),
        ~since=whole - 10,
        graph(21),
      )
      == (Whole(graph(21)), 0),
    );
  }),
  case("a late viewer gets one whole graph, where the last one was", () => {
    let change =
      switch (Delta.diff(graph(20), graph(21))) {
      | Some(change) => change
      | None => Alcotest.fail("no change")
      };
    Alcotest.check(
      strings,
      "events",
      texts([
        build(1),
        build(3),
        graph_line(4, Whole(graph(21))),
        build(5),
      ]),
      texts(
        Graphs.collapse([
          build(1),
          graph_line(2, Whole(graph(20))),
          build(3),
          graph_line(4, Change(change)),
          build(5),
        ]),
      ),
    );
  }),
  case("a trace without graphs is sent as it is", () =>
    Alcotest.check(
      strings,
      "events",
      texts([build(1), build(2)]),
      texts(Graphs.collapse([build(1), build(2)])),
    )
  ),
  case("changes with no whole graph before them are left out", () =>
    Alcotest.check(
      strings,
      "events",
      texts([build(1), build(3)]),
      texts(
        Graphs.collapse([
          build(1),
          graph_line(2, Change(parse({|{"drop":["edges"]}|}))),
          build(3),
        ]),
      ),
    )
  ),
  case("a compacted trace is smaller and ends with the same graph", () => {
    let whole =
      [build(0)]
      @ List.init(8, i => graph_line(i + 1, Whole(graph(20 + i))))
      @ [build(9)];
    let compacted = Graphs.compact(whole);
    let bytes = lines =>
      List.fold_left(
        (n, text) => n + String.length(text),
        0,
        texts(lines),
      );
    Alcotest.(check(bool))(
      "smaller",
      true,
      bytes(compacted) < bytes(whole),
    );
    Alcotest.(check(int))(
      "as many events",
      List.length(whole),
      List.length(compacted),
    );
    Alcotest.(check(bool))(
      "the first graph stays whole",
      true,
      Event.Line.draws(List.nth(compacted, 1)) == Some(`Whole),
    );
    Alcotest.(check(bool))(
      "the same graph at the end",
      true,
      latest(compacted) == Some(Whole(graph(27))),
    );
    Alcotest.check(
      strings,
      "compacting again changes nothing",
      texts(compacted),
      texts(Graphs.compact(compacted)),
    );
  }),
];
