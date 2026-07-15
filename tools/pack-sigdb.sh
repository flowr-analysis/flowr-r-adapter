#!/usr/bin/env bash
# Pack flowR's signature database into the release assets the R side downloads.
#
# Used by BOTH the binaries workflow and `make sigdb`, so what CI publishes is
# exactly what you can build and inspect locally.
#
#   tools/pack-sigdb.sh <flowr-version> [outdir] [scope ...]
#
# The database is data, not code: one archive per set serves every platform and
# every engine. The three sets are non-redundant and combine -- mount any subset,
# and flowR uses the richest thing it finds:
#
#   base     ~1 MB   base R only
#   current  ~24 MB  base R + every current CRAN package        (the default)
#   history  ~35 MB  the version history; supplements `current`, and carries no
#                    base-R signatures of its own, so it is an addition to it
#                    rather than a replacement
#
# What goes in is not guessed: flowR commits a pointer (`sigdb.remote.json`)
# naming the source release and every shard's sha256. We take the `.br` variants
# only -- brotli decompresses on every runtime, `.zst` needs zstd support, and
# shipping both would double the asset for no gain.
set -euo pipefail

version=${1:-}
outdir=${2:-out}
shift $(( $# > 2 ? 2 : $# )) || true
scopes=("$@")
[ ${#scopes[@]} -gt 0 ] || scopes=(base current history)

if [ -z "$version" ]; then
  echo "usage: $0 <flowr-version> [outdir] [scope ...]" >&2
  exit 2
fi
for t in jq curl sha256sum tar; do
  command -v "$t" >/dev/null || { echo "error: '$t' is required" >&2; exit 1; }
done

say() { echo "[pack-sigdb] $*"; }

work=${SIGDB_WORK:-.sigdb-work}          # shard cache: re-runs skip what is already correct
mkdir -p "$work" "$outdir"

# --- the pointer flowR ships -------------------------------------------------
# Prefer an installed flowR (CI has one); otherwise fetch just that file from the
# npm tarball, so a local run needs no node_modules.
ptr=node_modules/@eagleoutice/flowr/data/sigdb/sigdb.remote.json
if [ ! -f "$ptr" ]; then
  ptr="$work/sigdb.remote.json"
  if [ ! -f "$ptr" ]; then
    say "fetching flowR $version's sigdb pointer from npm"
    tgz=$(curl -fsSL "https://registry.npmjs.org/@eagleoutice/flowr/$version" | jq -r '.dist.tarball')
    [ -n "$tgz" ] && [ "$tgz" != "null" ] || { echo "error: no npm release for flowR $version" >&2; exit 1; }
    curl -fsSL "$tgz" -o "$work/flowr.tgz"
    tar -xzf "$work/flowr.tgz" -C "$work" package/data/sigdb/sigdb.remote.json
    mv "$work/package/data/sigdb/sigdb.remote.json" "$ptr"
    rm -rf "$work/package" "$work/flowr.tgz"
  fi
fi

tag=$(jq -r '.tag' "$ptr")
repo=$(jq -r '.repo // "flowr-analysis/flowr"' "$ptr")
[ -n "$tag" ] && [ "$tag" != "null" ] || { echo "error: no tag in $ptr" >&2; exit 1; }
say "source: $repo @ $tag  (pointer: $ptr)"

# --- shard prefix per set ----------------------------------------------------
# One prefix each: the sets are non-redundant, so nothing is packed twice.
prefix_for() {
  case "$1" in
    base|current|history) echo "$1." ;;
    *) echo "error: unknown scope '$1' (want base|current|history)" >&2; exit 2 ;;
  esac
}

# --- fetch + verify a shard --------------------------------------------------
# The pointer's sha256 is the integrity check: a mismatch is fatal, never shipped.
# An already-cached shard with the right hash is not re-downloaded.
fetch() {
  local name=$1 want dest got
  want=$(jq -r --arg n "$name" '.shards[$n].sha256 // empty' "$ptr")
  [ -n "$want" ] || { echo "error: $name is not in the pointer" >&2; exit 1; }
  dest="$work/$name"
  if [ -f "$dest" ] && [ "$(sha256sum "$dest" | cut -d' ' -f1)" = "$want" ]; then
    return 0
  fi
  curl -fsSL "https://github.com/$repo/releases/download/$tag/$name" -o "$dest"
  got=$(sha256sum "$dest" | cut -d' ' -f1)
  if [ "$got" != "$want" ]; then
    rm -f "$dest"
    echo "error: sha256 mismatch for $name (want $want, got $got)" >&2
    exit 1
  fi
}

# --- mount check -------------------------------------------------------------
# Hashes only prove we got the bytes the pointer named; they cannot tell us the
# result is a database flowR will actually mount. So boot a real flowR against
# the FINISHED archive, unpacked into a directory of its own, and require it to
# report a sigdb instead of "no sigdb".
#
# The isolation is the point, not tidiness: flowR searches a root and then walks
# up ten levels, so verifying in a directory nested under the shard cache mounts
# the cache instead of the artifact and passes no matter what was packed.
# Uses whichever flowR is at hand (the shipped bundle locally, node_modules in
# CI); skipped with a warning when there is none, never silently passed.
flowr_cli() {
  if [ -f node_modules/@eagleoutice/flowr/cli/flowr.js ]; then
    echo node_modules/@eagleoutice/flowr/cli/flowr.js
  elif [ -f inst/flowr-js/flowr.min.js ]; then
    echo inst/flowr-js/flowr.min.js
  fi
}

verify_mount() {
  local tgz=$1 cli wr wt out dir expect=$2
  cli=$(flowr_cli)
  if [ -z "$cli" ] || ! command -v node >/dev/null; then
    say "WARNING: no flowR/node here; skipping the mount check for $tgz"
    return 0
  fi
  dir=$(mktemp -d)                       # outside the cache: see the note above
  trap 'rm -rf "$dir"' RETURN
  tar -xzf "$tgz" -C "$dir"
  wr=$(dirname "$cli")/tree-sitter-r.wasm
  wt=$(dirname "$cli")/tree-sitter.wasm
  [ -f "$wr" ] || wr=$(find node_modules -name 'tree-sitter-r.wasm' -print -quit 2>/dev/null || true)
  [ -f "$wt" ] || wt=$(find node_modules -name 'tree-sitter.wasm'   -print -quit 2>/dev/null || true)
  out=$(printf ':quit\n' | FLOWR_SIGDB_DIR="$dir" node "$cli" \
          --default-engine tree-sitter --engine.r-shell.disabled \
          --engine.tree-sitter.wasm-path "$wr" \
          --engine.tree-sitter.tree-sitter-wasm-path "$wt" 2>&1 | head -1 || true)
  out=$(echo "$out" | sed 's/\x1b\[[0-9;]*m//g; s/\x1b]8;;[^\a]*\a//g')
  if echo "$out" | grep -q "no sigdb" || ! echo "$out" | grep -q "sigdb:"; then
    echo "error: the packed database does not mount:" >&2
    echo "$out" >&2
    exit 1
  fi
  # Require the scope we packed, not merely *a* database: anything else means we
  # mounted something other than this artifact.
  if ! echo "$out" | grep -q "sigdb: *$expect"; then
    echo "error: packed '$expect' but flowR mounted something else:" >&2
    echo "$out" >&2
    exit 1
  fi
  say "  mounts: $(echo "$out" | grep -o 'sigdb:[^)]*')"
}

# --- pack --------------------------------------------------------------------
for scope in "${scopes[@]}"; do
  stage="$work/stage-$scope"
  rm -rf "$stage"; mkdir -p "$stage"
  n=0
  p=$(prefix_for "$scope")
  while read -r name; do
    [ -n "$name" ] || continue
    fetch "$name"
    cp "$work/$name" "$stage/"
    n=$((n + 1))
  done < <(jq -r --arg p "$p" \
    '.shards | keys[] | select(startswith($p)) | select(endswith(".br"))' "$ptr")
  [ "$n" -gt 0 ] || { echo "error: scope '$scope' matched no shards" >&2; exit 1; }
  ls "$stage"/*.manifest.json.br >/dev/null 2>&1 || {
    echo "error: scope '$scope' has no manifest; it would not mount" >&2; exit 1; }

  tgz="$outdir/flowr-sigdb-$scope-$version.tar.gz"
  tar -czf "$tgz" -C "$stage" .
  ( cd "$outdir" && sha256sum "$(basename "$tgz")" > "$(basename "$tgz").sha256" )
  say "$(basename "$tgz")  ($n shards, $(du -h "$tgz" | cut -f1))"
  # what flowR calls each set; `history` presents itself as the full history
  case "$scope" in history) expect="full-history" ;; *) expect="$scope" ;; esac
  verify_mount "$tgz" "$expect"
  rm -rf "$stage"
done

say "done -> $outdir"
