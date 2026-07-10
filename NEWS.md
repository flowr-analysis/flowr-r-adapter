# flowr 0.2.2

# flowr 0.2.1

# flowr 0.2.0

A major (AI-assisted) rewrite of the adapter into a robust, engine-selectable
flowR client. flowR itself is unchanged; this package wraps it.

## High-level API

* `slice()`, `query()`, `dataflow()`, `flowr_overview()`, `inspect_project()`
  and `flowr_repl()` work with no manual setup, backed by a lazily created,
  reused default session (`library(flowr); slice(code, criterion)`).
* Slicing accepts several criteria at once, including flowR's `";"`-separated
  form (`"3@print;2@y"`), and warns when a criterion matches nothing.
* Project-aware commands default to the enclosing R project (a `DESCRIPTION`,
  an `.Rproj`, a git root, or a folder of `.R` files) and report the root they
  pick; `inspect_project()` summarises a project's files and their roles.
* `slice()` prints a colored git-style diff by default; `style = "gray"` dims
  unused lines and `style = "code"` shows just the slice. Colour honours
  `NO_COLOR` and `options(flowr.color = )`.
* `flowr_overview()` lists a script's libraries, sources, reads/writes, plots
  and tests, each with a slicing criterion; `flowr_graph()` /
  `flowr_as_igraph()` turn the dataflow or call graph into an igraph.

## Engines

* Interchangeable, selectable engines behind a small registry: `bundled`
  (flowR's JS+wasm shipped in the package, runs on your Node, no download),
  `binary` (self-contained native executable), `node` and `docker`. `auto`
  prefers a cached binary, then the bundle, then the binary.
* `flowr_install()`, `flowr_installed("all")`, `flowr_uninstall()` and
  `flowr_update()` (which also checks for a newer flowR) manage engines.
* `flowr_console()` hands the terminal to flowR's native REPL (history and tab
  completion); `flowr_console(r_access = TRUE)` also runs R via the r-shell
  engine. `flowr_watch()` re-runs a command when a file changes (flowR's
  `watch://`).

## Configuration and diagnostics

* `flowr_config()` shows the effective, grouped configuration (server-only and
  engine-specific settings are marked); `flowr_set_config()` changes settings
  for the session or persists them. Settings also resolve from
  `options(flowr.*)`, `FLOWR_*` environment variables and a JSON config file.
* `flowr_status()` reports the engine, backend, and the installed binary's
  verification state, age and hash. `flowr_log()` returns the flowR server log;
  `options(flowr.timing = TRUE)` prints per-command timings (with flowR's own
  phase breakdown) and `options(flowr.verbose = TRUE)` runs flowR verbosely.
* `flowr_bug_report()` opens a prefilled GitHub issue; `flowr_feedback()` opens
  the shared feedback form.

## Security

* Secure mode is on by default: the tree-sitter parser only (untrusted code is
  never handed to an R interpreter), loopback-only server connections, and no
  shell invocation (processes start from an argument vector).
* Downloaded binaries are SHA-256-verified before execution and, when a public
  key is pinned, signature-verified against it (openssl, no gpg needed); in
  secure mode signature verification cannot be silently disabled.

## Internals

* Rewritten socket protocol client with real timeouts, buffered reassembly, EOF
  detection and REPL streaming; bind-verified free-port selection.
* One warm server is reused across calls and analyses are cached per content, so
  repeated operations avoid re-spawning and re-analysis.
* Minimal footprint: only `jsonlite` and `sys` are required (`digest` and
  `openssl` are suggested, used only for verifying binary downloads). Extensive
  regression, functionality and resource-leak tests.
* Targets flowR 2.11.1, including the full query API and the REPL.

# flowr 0.0.3

# flowr 0.0.2

# flowr 0.0.1

* Added a `NEWS.md` file to track changes to the package.
