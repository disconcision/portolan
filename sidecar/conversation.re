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
  /* The last event of a kind, to open the new trace with. */
  let carried = kind => Option.to_list(Bus.last(kind));
  Graphs.starting_over(graph =>
    Bus.start_over(
      ~archive=Project.state(project, earlier),
      [Event.Session({restart: false})]
      @ carried(Hello)
      @ [Event.Conversation({earlier: earlier})]
      @ carried(Build)
      @ Option.to_list(graph)
      @ carried(Tests)
      @ [Event.Agent_state(Idle)],
    )
  );
};
