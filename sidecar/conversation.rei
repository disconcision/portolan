/* Starting a new conversation in a project. */

/** Ends the agent's conversation and starts a new trace. The trace so
    far is kept as `.portolan/conversations/<UTC time>.jsonl`, which
    `portolan replay` can play. The new trace opens with what a viewer
    needs to carry on: the project, the last build and test results, the
    current graph, and a `conversation {earlier}` event naming the kept
    file relative to `.portolan`. */
let start_new: (Project.t, Agent.t) => unit;
