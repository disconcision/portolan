#!/usr/bin/env python3
"""A code base as it stands, as a trace of one event that the page can show with no sidecar.

  tools/snapshot.py <project-dir> <out.jsonl> [--name NAME] [--build-dir DIR] [--extract EXE]

Runs the extractor on a project that is already built (its annotations are in DIR, default
_build_portolan), and writes: the graph, and the text of every file the graph mentions, so the
code panel works. Paths are relative to the project, so nothing names the machine.
"""
import json, os, subprocess, sys

args = sys.argv[1:]
def flag(name, default):
    if name in args:
        i = args.index(name); value = args[i + 1]; del args[i:i + 2]; return value
    return default
build_dir = flag("--build-dir", "_build_portolan")
here = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
extract = flag("--extract", os.path.join(here, "_build", "default", "extract", "main.exe"))
name = flag("--name", None)
if len(args) != 2:
    sys.exit(__doc__)
project, out = os.path.realpath(args[0]), args[1]
name = name or os.path.basename(project)

graph = json.loads(subprocess.run([extract, project, "--build-dir", build_dir], check=True, capture_output=True, text=True).stdout)
mentioned = set()
for kind in ("nodes", "edges", "values", "loose_tests"):
    for x in graph.get(kind, []):
        for t in [x] + x.get("tests", []):
            if t.get("at"):
                mentioned.add(t["at"]["file"])
files = {}
for f in sorted(mentioned):
    p = os.path.join(project, f)
    if os.path.isfile(p) and os.path.getsize(p) < 400_000:
        files[f] = open(p).read()

events = [
    {"t": 0, "type": "hello", "project": name, "agent": "none", "mode": "snapshot"},
    {"t": 1, "type": "graph", "graph": graph, "cause": {"label": "snapshot", "by": "hand"}},
    {"t": 2, "type": "build", "ok": True, "output": "", "ms": 0},
    {"t": 3, "type": "files", "files": files},
]
with open(out, "w") as f:
    for e in events:
        f.write(json.dumps(e, separators=(",", ":")) + "\n")
n = sum(len(graph.get(k, [])) for k in ("nodes", "edges", "values"))
print(f"{name}: {n} graph items, {len(files)} files, {os.path.getsize(out) // 1024} KB")
