#!/usr/bin/env sh
# Run a command with the rv-managed package library on R's search path.
#
# The Makefile invokes R with --no-init-file (so dev tools resolve regardless of
# rv activation), which means rv's .Rprofile is skipped and the locked library
# is NOT on .libPaths() by default. This wrapper asks rv where that library is
# and exports it via R_LIBS_USER so testthat/knitr/roxygen2/etc. are found.
#
# Usage: tools/with-rvlib.sh <command> [args...]
set -eu

ROOT="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"

if command -v rv >/dev/null 2>&1; then
  RVLIB="$(rv library 2>/dev/null || true)"
  if [ -n "$RVLIB" ]; then
    # rv prints a repo-relative path; make it absolute.
    case "$RVLIB" in
      /*) ;;
      *) RVLIB="$ROOT/$RVLIB" ;;
    esac
    if [ -d "$RVLIB" ]; then
      export R_LIBS_USER="$RVLIB${R_LIBS_USER:+:$R_LIBS_USER}"
    else
      echo "[with-rvlib] rv library '$RVLIB' does not exist; run 'make sync' first." >&2
    fi
  fi
else
  echo "[with-rvlib] 'rv' not found on PATH; relying on the default R library." >&2
fi

exec "$@"
