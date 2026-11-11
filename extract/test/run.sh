#!/bin/sh
# Builds the annotations of the examples, then runs the extractor's tests.
# The tests that break a build do it in copies they make under a temporary
# directory. Set PORTOLAN_SWITCH to run in a particular opam switch, and
# PORTOLAN_BUILD_DIR to build Portolan somewhere other than _build.
set -e
cd "$(dirname "$0")/../.."
if [ -n "$PORTOLAN_SWITCH" ]; then
  eval "$(opam env --switch="$PORTOLAN_SWITCH" --set-switch)"
fi
build_dir="${PORTOLAN_BUILD_DIR:-_build}"
for example in vector shapes plane checks; do
  (cd "examples/$example" && dune build --root . --build-dir _build_portolan @check)
done
dune build --build-dir "$build_dir" ./extract
mkdir -p "$build_dir/_tests"
PORTOLAN_ROOT="$PWD" "./$build_dir/default/extract/test/test_extract.exe" "$@" -o "$PWD/$build_dir/_tests"
