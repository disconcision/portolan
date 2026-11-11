#!/usr/bin/env python3
"""Make a recorded trace safe to publish, and self-contained.

  tools/scrub_trace.py <trace.jsonl> <project-dir> <out.jsonl>

Removes the session's system events (they list the machine's tools and
settings), ids, signatures and costs; rewrites absolute paths so nothing
names the machine; and appends the project's final source files so the
replay page can show code without a sidecar.
"""
import json, os, sys

trace, project, out = sys.argv[1], os.path.realpath(sys.argv[2]), sys.argv[3]
home = os.path.expanduser("~")
user = os.path.basename(home)

def clean_text(s):
    return s.replace(project + "/", "").replace(project, ".").replace(home, "~").replace(user, "user")

def walk(x):
    if isinstance(x, str):
        return clean_text(x)
    if isinstance(x, list):
        return [walk(v) for v in x]
    if isinstance(x, dict):
        return {k: walk(v) for k, v in x.items()
                if k not in ("session_id", "uuid", "signature", "usage", "modelUsage", "total_cost_usd",
                             "parent_tool_use_id", "permission_denials", "fast_mode_state", "request_id",
                             "tool_use_result", "timestamp")}
    return x

kept = []
for line in open(trace):
    line = line.strip()
    if not line:
        continue
    e = json.loads(line)
    if e.get("type") == "agent_raw":
        continue
    if e.get("type") == "agent":
        ev = e.get("ev", {})
        t = ev.get("type")
        if t == "system":
            continue
        if t == "stream_event":
            inner = ev.get("event", {})
            it = inner.get("type")
            block = inner.get("content_block", {}).get("type")
            delta = inner.get("delta", {}).get("type")
            if it == "content_block_start" and block in ("text", "tool_use"):
                pass
            elif it == "content_block_delta" and delta == "text_delta":
                pass
            elif it == "content_block_delta" and delta == "input_json_delta":
                # Paths arrive split across chunks, so keep only the length.
                inner["delta"]["partial_json"] = " " * len(inner["delta"].get("partial_json", ""))
            else:
                continue
        if t == "result":
            e["ev"] = {"type": "result", "subtype": ev.get("subtype"), "is_error": ev.get("is_error", False)}
    kept.append(walk(e))

files = {}
for root, dirs, names in os.walk(project):
    dirs[:] = [d for d in dirs if not d.startswith((".", "_"))]
    for n in names:
        if n.endswith((".ml", ".mli", ".re", ".rei")) or n in ("dune", "dune-project"):
            p = os.path.join(root, n)
            if os.path.getsize(p) < 200_000:
                files[os.path.relpath(p, project)] = open(p).read()
kept.append({"t": 0, "type": "files", "files": files})

with open(out, "w") as f:
    for e in kept:
        f.write(json.dumps(e, separators=(",", ":")) + "\n")
print(f"{len(kept)} events, {os.path.getsize(out) // 1024} KB, {len(files)} files")
