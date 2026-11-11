# How code is written here

A light guide for agents and people working on Portolan's own code. These are leanings, not laws. Where one fights the code in front of you, go the other way and say why in the commit. It is expected to change as we learn what works, partly from what the canvas shows about our own modules.

## Types carry the design

- **Name the things of the domain.** If two functions pass the same string or int with the same meaning (a node key, a qualified name, a path in the project, an event), give it a type, abstract if it has a rule to keep. A function from `string` to `string` tells a reader nothing.
- **A state is a variant, not a record of options.** If only some combinations of `None` and `Some` make sense, write the combinations down as constructors.
- **Parse at the edge.** Text and JSON become typed values in one place; past that point nothing handles `Yojson.Safe.t` or raw text. The same on the way out.
- **One definition of each model.** Two modules that both need a record share it, or one converts at the boundary. No parallel copies kept in step by hand.
- **Expected failure is in the type** (`option`, `result`); exceptions are for bugs. The interface says which a function does.

## Modules have one job and an interface

- **One job, sayable in a sentence**, at the top of the `.rei`. Each value in the interface says what it does. If the sentence needs an "and", consider two modules.
- **Abstract by default.** Expose a type's shape only when callers need it.
- **State is a value.** Prefer a `t` that is passed in over module-level `ref`s. A module that is one-per-process by nature (the event bus) says so at the top of its interface.
- **Effects at the rim.** Keep the part that decides (parsing, comparing, choosing) apart from the part that reads files, runs commands or emits events, so the first can be tested without the second.
- **Dependencies point one way.** Small modules (`Files`, `Run`) know nothing of the ones built on them.
- **Nested modules and functors when they earn it:** a nested module for a type with its handful of operations; a functor when there are two real uses. Not for show.

## Tests sit beside what they check

- **A function with a decision in it has a test**, named for the behaviour ("a broken module keeps its definitions"), written against the interface.
- **A bug fix starts with the test that fails.**
- **Glue needs no test of its own**; it is covered by running the thing.
- Tests show on the canvas as pips on the function they exercise, so untested logic is visible.

## Changes

- When a change wants a new type or module, add it; do not thread one more string or flag through.
- Each commit builds and passes the tests. Small commits, each saying what changed and why.
- Comments are short and say why, not what.

## What the canvas shows

Reading our own modules on the canvas, these shapes have meant something each time so far:

| On the canvas | Usually means |
|---|---|
| Many small islands, built-in type to built-in type | No domain types: the functions share nothing but `string` and `int` |
| A fan of arrows into `unit` | Effects with no result; check whether the deciding part can be split out |
| A record with a ring of `option` parts | A state machine written as optional fields |
| Loose values of `ref`, `Mutex`, table types | Module-level state |
| The same cluster of small helpers in two modules | A missing shared module |
| Two modules with records of the same name | One model defined twice |
