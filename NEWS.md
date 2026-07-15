# flowr 0.2.8

* Targets flowR 2.12.3 (from 2.11.1); the shipped `inst/flowr-js` bundle is
  rebuilt accordingly.
* `flowr_install()` now also obtains flowR's signature database, so `library()`
  and `::` exports (base R plus CRAN) resolve instead of flowR reporting
  `no sigdb`. The new `sigdb` argument selects which sets to fetch --- `"base"`
  (~1 MB), `"current"` (~24 MB, the default), `"history"` (~35 MB), `"all"` or
  `"none"` --- and defaults to the new `flowr.sigdb` option, so
  `options(flowr.sigdb = "none")` turns it off. The sets are non-redundant and
  mount together.
  The database is platform-independent, shared by every engine, and published
  beside the binaries with the same verification (checksum, plus a signature
  against the pinned key --- mandatory in secure mode). It is obtained
  independently of the engine, so an installed engine is left alone and
  `flowr_install(sigdb = "history")` just adds that set. Failing to obtain it
  warns rather than failing the install, and nothing is downloaded unless you
  ask: an analysis never fetches it. `flowr_status()` reports which sets are
  present and how they were verified.
* In an interactive session, `flowr_install()` now asks which signature-database
  sets to download when you pass no `sigdb` and have not set the `flowr.sigdb`
  option --- the choice spans ~35 MB, so it is worth a question. Passing the
  argument or setting the option settles it and is never asked about again, and
  unattended sessions never prompt.
* `flowr_status()` mentions a newer flowR or flowr release when one exists. It is
  silent when you are up to date, silent when it cannot check (offline, DNS
  failure, a rate-limited API), never errors or warns, and gives up after a few
  seconds rather than stalling a session. It asks at most once per session; set
  `options(flowr.check_updates = FALSE)` to never look.
* `flowr_uninstall()` gains `engine` and `sigdb` arguments mirroring
  `flowr_install()`, so you can select what to remove instead of clearing
  everything: `flowr_uninstall(engine = "none", sigdb = "history")` drops one
  database set, `flowr_uninstall(sigdb = "none")` removes only the engines. The
  default still removes everything, and `version = NULL` now applies the
  selection across every cached version.
* `slice()` gains two flowR 2.12 options. `include_callees = TRUE` continues a
  backward slice past a function-definition boundary, also pulling in the
  definition's binding and call sites (flowR ignores it for forward slices, so
  `slice()` warns instead of quietly returning an unchanged slice).
  `inline_sources = TRUE` inlines resolvable `source()` calls so the slice is a
  single self-contained script; calls that are cyclic or unresolvable are kept
  verbatim and reported in the new `inline_warnings` element.

# flowr 0.2.7

# flowr 0.2.6

# flowr 0.2.5

# flowr 0.2.4

* New `flowr_lint()` runs flowR's linter and returns findings as a
  lintr-compatible data frame; `format = "sarif"`/`"github"`/`"jarl"` emit the
  machine-readable outputs that jarl produces. `flowr_lint_fix()` applies flowR's
  quick fixes (in place, or returning corrected code for a snippet).
* `flowr_installed()` is renamed to `flowr_is_installed()` (no alias is kept).
* `inspect_project()` now also reports the flowR version and a dependency
  summary, not just the analysed files.
* New `vignette("flowr-security")` explains binary integrity and how to turn on
  signature verification; `flowr_status()` points there when a binary is
  checksum-verified only.

# flowr 0.2.3

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
* `flowr_install()`, `flowr_is_installed("all")`, `flowr_uninstall()` and
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
