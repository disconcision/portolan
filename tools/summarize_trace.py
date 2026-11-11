#!/usr/bin/env python3
"""One line per thing that happened in a trace, then totals.

  tools/summarize_trace.py <trace.jsonl> [--quiet]
"""
import json, sys

ev = [json.loads(l) for l in open(sys.argv[1]) if l.strip()]
quiet = "--quiet" in sys.argv
out = (lambda *a: None) if quiet else print
prev, graphs, adds, writes, edits, fails, fail_start, broken_ms, done = set(), 0, [], [], 0, 0, None, 0, None
for e in ev:
    t, ty = e.get("t", 0) / 1000, e["type"]
    if ty == "graph":
        g = e["graph"]
        ids = {("n", n["key"]) for n in g["nodes"] if n["kind"] == "alias"} | {("e", x["name"]) for x in g["edges"]}
        add, rem = ids - prev, prev - ids
        prev = ids
        graphs += 1
        if add: adds.append(len(add))
        out(f"{t:7.1f}s graph   {e['cause'].get('label')} {e['cause'].get('file', '')}  +{len(add)} -{len(rem)}  "
            f"types={sum(1 for n in g['nodes'] if n['kind'] == 'alias')} fns={len(g['edges'])} "
            f"tests={sum(len(x['tests']) for x in g['edges'])}  {sorted(x[1].split('.')[-1] for x in add)}")
    elif ty == "build":
        out(f"{t:7.1f}s build   {'ok' if e['ok'] else 'FAIL'} {e['ms']} ms")
        if not e["ok"]:
            fails += 1
            fail_start = fail_start if fail_start is not None else e["t"]
        elif fail_start is not None:
            broken_ms += e["t"] - fail_start
            fail_start = None
    elif ty == "tests":
        out(f"{t:7.1f}s tests   {len(e['failing'])} failing")
    elif ty == "agent":
        a = e["ev"]
        if a.get("type") == "assistant":
            for b in a["message"]["content"]:
                if b["type"] != "tool_use": continue
                i, n = b["input"], b["name"]
                if n == "Write":
                    k = len(i.get("content", "").splitlines()); writes.append(k)
                    out(f"{t:7.1f}s   Write {i.get('file_path', '').split('/')[-1]} {k} lines")
                elif n == "Edit":
                    edits += 1
                    out(f"{t:7.1f}s   Edit  {i.get('file_path', '').split('/')[-1]} {len(i.get('old_string', '').splitlines())} -> {len(i.get('new_string', '').splitlines())} lines")
                else:
                    out(f"{t:7.1f}s   {n} {str(i.get('command', ''))[:70]}")
        elif a.get("type") == "result":
            done = e["t"] / 1000
if fail_start is not None:
    broken_ms += ev[-1].get("t", 0) - fail_start  # still broken at the end
print(f"\n{(done or ev[-1].get('t', 0) / 1000):.0f} s · {graphs} graph changes · definitions arriving per change: {adds} · "
      f"{len(writes)} whole-file writes {writes} · {edits} edits · {fails} failed builds, broken for {broken_ms / 1000:.0f} s")
