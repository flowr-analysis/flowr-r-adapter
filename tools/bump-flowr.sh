#!/usr/bin/env bash
# Retarget the package to another flowR version: rebuild the shipped
# inst/flowr-js bundle, replace the old version everywhere it is named and
# record the bump in NEWS.md. Used by the flowr-update workflow; works locally.
# usage: tools/bump-flowr.sh <flowr-version>
set -euo pipefail

new="${1:?usage: tools/bump-flowr.sh <flowr-version>}"
[[ "$new" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]] || { echo "expected a flowR version, got '$new'" >&2; exit 1; }

cd "$(git rev-parse --show-toplevel)"
old=$(sed -n -E 's/.*flowr_version[[:space:]]*=[[:space:]]*"([^"]*)".*/\1/p' R/config.R | head -1)
[ -n "$old" ] || { echo "could not read flowr_version from R/config.R" >&2; exit 1; }
[ "$old" != "$new" ] || { echo "already targets flowR $new"; exit 0; }

# build the bundle outside the repository so no node_modules end up in it
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
(
  cd "$work"
  npm init -y >/dev/null
  # install scripts only build native tree-sitter bindings; the bundle uses the wasm
  npm install --no-fund --no-audit --ignore-scripts "@eagleoutice/flowr@$new" ignore esbuild
  npx esbuild --bundle node_modules/@eagleoutice/flowr/cli/flowr.js \
    --platform=node --minify --legal-comments=none --target=node18 \
    --outfile=flowr.min.js
)
cp "$work/flowr.min.js" inst/flowr-js/
cp "$work/node_modules/@davisvaughan/tree-sitter-r/tree-sitter-r.wasm" inst/flowr-js/
cp "$work/node_modules/web-tree-sitter/tree-sitter.wasm" inst/flowr-js/
printf 'VERSION %s\n' "$new" > inst/flowr-js/VERSION

# the default, the docs and the tests name the version. Workflow files are left
# alone: pushing a change to one needs a token with the `workflow` scope, and the
# flowr-update workflow should not depend on that.
git grep -lF "$old" -- . ':!NEWS.md' ':!inst/flowr-js' ':!.github' \
  | xargs -r perl -pi -e "s/\\Q$old\\E/$new/g"

# NEWS.md: amend the pending entry of the development version, or start one
OLD="$old" NEW="$new" perl -0pi -e '
  my ($old, $new) = @ENV{qw(OLD NEW)};
  my $entry = "* Targets flowR $new (from $old); the shipped `inst/flowr-js` bundle is\n  rebuilt accordingly.\n";
  if (/\A# flowr \(development version\)\n/) {
    s/^\* Targets flowR \S+ \(from (\S+)\);/* Targets flowR $new (from $1);/m
      or s/\A(# flowr \(development version\)\n\n)/$1$entry/;
  } else {
    $_ = "# flowr (development version)\n\n$entry\n$_";
  }
' NEWS.md

echo "bumped flowR $old -> $new"
