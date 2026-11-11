#!/bin/sh
# Builds the annotations of both examples, then runs the extractor's tests.
# Set PORTOLAN_SWITCH to run in a particular opam switch.
set -e
cd "$(dirname "$0")/../.."
if [ -n "$PORTOLAN_SWITCH" ]; then
  eval "$(opam env --switch="$PORTOLAN_SWITCH" --set-switch)"
fi
for example in vector shapes; do
  (cd "examples/$example" && dune build --root . --build-dir _build_portolan @check)
done
dune build ./extract
PORTOLAN_ROOT="$PWD" ./_build/default/extract/test/test_extract.exe "$@"
