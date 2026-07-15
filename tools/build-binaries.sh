#!/usr/bin/env bash
# Compile a self-contained flowR binary for THIS machine, the same way the
# binaries workflow does, so you can build and try one without a CI round-trip.
#
#   tools/build-binaries.sh [flowr-version] [outdir]
#
# CI publishes five platforms; this builds only the host's. Cross-compiling is
# deliberately not offered here: a Windows .exe cross-compiled from Linux
# segfaults at launch (a Bun bug), which is why the workflow builds win-x64 on a
# Windows runner. Build on the platform you want, or let CI do it.
#
# Needs npm (bun is fetched through it if absent, into a local prefix -- nothing
# is installed globally).
set -euo pipefail

version=${1:-}
outdir=${2:-out}
if [ -z "$version" ]; then
  version=$(sed -n -E 's/.*flowr_version[[:space:]]*=[[:space:]]*"([^"]*)".*/\1/p' R/config.R | head -1)
fi
[ -n "$version" ] || { echo "usage: $0 <flowr-version> [outdir]" >&2; exit 2; }
command -v npm >/dev/null || { echo "error: npm is required" >&2; exit 1; }

say() { echo "[build-binaries] $*"; }

work=${BUILD_WORK:-.binary-work}
mkdir -p "$work" "$outdir"
# absolute from here on: the build cds into $work, and bun is invoked by path
work=$(cd "$work" && pwd)

# Same pin as the workflow: the compiled program's argv layout is Bun-version
# dependent on Windows, so the version is not left to whatever is on PATH.
BUN_VERSION=1.3.14
bun=$(command -v bun || true)
if [ -z "$bun" ]; then
  say "no bun on PATH; fetching bun@$BUN_VERSION into $work"
  ( cd "$work" && npm install --no-fund --no-audit --silent "bun@$BUN_VERSION" )
  bun="$work/node_modules/.bin/bun"
fi
[ -x "$bun" ] || { echo "error: could not obtain bun ($bun)" >&2; exit 1; }
say "bun: $("$bun" --version)  (want $BUN_VERSION)"

# host -> release target name, matching the workflow's asset naming
os=$(uname -s); arch=$(uname -m)
case "$os" in
  Linux)  o=linux ;;
  Darwin) o=darwin ;;
  MINGW*|MSYS*|CYGWIN*) o=win ;;
  *) echo "error: unsupported OS '$os'" >&2; exit 1 ;;
esac
case "$arch" in
  x86_64|amd64) a=x64 ;;
  arm64|aarch64) a=arm64 ;;
  *) echo "error: unsupported arch '$arch'" >&2; exit 1 ;;
esac
target="$o-$a"
ext=""; [ "$o" = "win" ] && ext=".exe"
say "building flowR $version for $target"

# `ignore` is a peer import of flowR's CLI that bun must resolve when compiling.
( cd "$work" && printf '{}\n' > package.json \
    && "$bun" add --silent "@eagleoutice/flowr@$version" ignore )

entry="$work/node_modules/@eagleoutice/flowr/cli/flowr.js"
[ -f "$entry" ] || { echo "error: flowR $version has no CLI at $entry" >&2; exit 1; }
wr=$(find "$work/node_modules" -name 'tree-sitter-r.wasm' -print -quit)
wt=$(find "$work/node_modules" -name 'tree-sitter.wasm'   -print -quit)
[ -n "$wr" ] && [ -n "$wt" ] || { echo "error: tree-sitter wasm not found" >&2; exit 1; }

dist="$work/dist"; rm -rf "$dist"; mkdir -p "$dist"
# No --bytecode: it embeds host JSC bytecode, which segfaults on a
# cross-compiled target. Kept off here too so a local build matches CI's output.
"$bun" build "$entry" --compile --minify --sourcemap=none \
  --target="bun-$o-$a" --outfile "$dist/flowr$ext"
cp "$wr" "$wt" "$dist/"

# Boot it, exactly as the workflow's smoke job does: a binary that does not start
# is not worth packaging.
say "smoke-testing the binary"
out=$(printf ':quit\n' | "$dist/flowr$ext" --default-engine tree-sitter \
        --engine.r-shell.disabled \
        --engine.tree-sitter.wasm-path "$dist/tree-sitter-r.wasm" \
        --engine.tree-sitter.tree-sitter-wasm-path "$dist/tree-sitter.wasm" 2>&1 | head -1 || true)
if ! echo "$out" | grep -q "flowR repl"; then
  echo "error: the built binary does not boot:" >&2; echo "$out" >&2; exit 1
fi
say "  $(echo "$out" | sed 's/\x1b\[[0-9;]*m//g')"

tgz="$outdir/flowr-$version-$target.tar.gz"
tar -czf "$tgz" -C "$dist" .
( cd "$outdir" && sha256sum "$(basename "$tgz")" > "$(basename "$tgz").sha256" )
say "$(basename "$tgz")  ($(du -h "$tgz" | cut -f1))"
say "done -> $outdir"
