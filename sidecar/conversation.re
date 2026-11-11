let (/) = Filename.concat;

/* "2026-11-11T07-12-33Z": sorts by time, safe in a file name. */
let stamp = () => {
  let t = Unix.gmtime(Unix.time());
  Printf.sprintf(
    "%04d-%02d-%02dT%02d-%02d-%02dZ",
    t.tm_year + 1900,
    t.tm_mon + 1,
    t.tm_mday,
    t.tm_hour,
    t.tm_min,
    t.tm_sec,
  );
};

let start_new = (project, agent) => {
  Agent.forget(agent);
  let earlier = "conversations" / (stamp() ++ ".jsonl");
  /* The last event of a type, to open the new trace with. */
  let carried = ty =>
    switch (Bus.last(ty)) {
    | Some(`Assoc(fields)) => [List.remove_assoc("t", fields)]
    | _ => []
    };
  Graphs.starting_over(graph =>
    Bus.start_over(
      ~archive=Project.state(project, earlier),
      [[("type", `String("session")), ("restart", `Bool(false))]]
      @ carried("hello")
      @ [
        [
          ("type", `String("conversation")),
          ("earlier", `String(earlier)),
        ],
      ]
      @ carried("build")
      @ Option.to_list(graph)
      @ carried("tests")
      @ [[("type", `String("agent_state")), ("state", `String("idle"))]],
    )
  );
};
