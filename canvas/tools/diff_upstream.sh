#!/bin/sh
# Compare the copied files with a Hazel checkout.
#
#   sh canvas/tools/diff_upstream.sh /path/to/hazel-worktree
#
# upstream/ must be identical; edited/ prints its (small) diff.
set -u
hazel=${1:?usage: diff_upstream.sh /path/to/hazel-worktree}
here=$(cd "$(dirname "$0")/.." && pwd)

src_of() {
  case "$1" in
    Animation.re) echo "src/web/$1" ;;
    GraphLayout.re|GraphLayout.rei|AgentPulse.re|PerfTimer.re|OptUtil.re) echo "src/util/$1" ;;
    *) echo "src/web/app/canvas/$1" ;;
  esac
}

status=0
for f in "$here"/upstream/*; do
  name=$(basename "$f")
  if cmp -s "$f" "$hazel/$(src_of "$name")"; then
    echo "same     upstream/$name"
  else
    echo "DIFFERS  upstream/$name  (vs $(src_of "$name"))"
    status=1
  fi
done
for f in "$here"/edited/*; do
  name=$(basename "$f")
  lines=$(diff "$hazel/$(src_of "$name")" "$f" | grep -c '^[<>]')
  echo "edited   edited/$name  ($lines changed lines vs $(src_of "$name"))"
  diff "$hazel/$(src_of "$name")" "$f" | sed 's/^/    /'
done
exit $status
