# flowr - call flowR from R

<!-- badges: start -->
<!-- badges: end -->

`flowr` is a robust R client for
[flowR](https://github.com/flowr-analysis/flowr), a static dataflow analyzer and
program slicer for the R language. It lets you slice R scripts, extract their
dependencies, lint them, run flowR's query API and drive its REPL - directly
from R.

```r
library(flowr)

code <- "x <- 1\ny <- 2\nz <- x + 5\nprint(z)"
cat(slice(code, "4@z")$code)
#> x <- 1
#> z <- x + 5

# lint (lintr-compatible columns; also SARIF / GitHub / jarl output),
# and apply flowR's quick fixes:
flowr_lint("setwd('/tmp')\nunused <- 1")
flowr_lint_fix(file = "analysis.R")
```

## Installation

```r
# install.packages("remotes")
remotes::install_github("flowr-analysis/flowr-r-adapter", build_vignettes = TRUE)
```

`build_vignettes = TRUE` is needed to get the vignettes (`remotes` skips them by
default); without it, `vignette("flowr")` is not installed and the startup
banner points at `?flowr` instead.

Installing from GitHub ships flowR's JS + wasm **bundle** inside the
package, so `slice()` works out of the box on any platform that has Node.js, with
no download. For a Node-free setup, fetch the self-contained native binary once:

```r
flowr::flowr_install()   # downloads + verifies the native binary for your platform
```

## What is downloaded, and how it is verified

Full disclosure of the package's network behaviour:

* **Nothing is downloaded on load.** The shipped bundle runs on your Node with no
  download. Any binary download happens only **on demand** - when you call
  `flowr_install()`, or (interactively) the first time `auto` needs it - and only
  after confirmation.
* Native binaries are fetched from this repository's GitHub **releases**
  (`flowr-v<version>` tags), picking the archive for your platform
  (`flowr-<version>-<os>-<arch>.tar.gz`).
* Every download is **SHA-256 checksum-verified** before it is unpacked or run.
* When the maintainer ships a public key (`inst/flowr-pubkey.pem`) and signs the
  release, the archive's **signature is verified against that pinned key** using
  the `openssl` package - **no `gpg` installation required**, and the check is
  deterministic. Missing/incorrect signatures are rejected in secure mode.
* Downloads are cached under `tools::R_user_dir("flowr", "cache")`.
* To avoid downloads entirely, use `engine = "bundled"` (Node) or `"docker"`.

> **Verification is not a substitute for trust.** Checksums and the signature
> prove the binary is *exactly what this project published* and was not tampered
> with in transit - nothing more. They do **not** vet what flowR (or this
> adapter) actually does, and running any analysis engine means running its code
> on your machine. Install and run flowR only if you trust the project, the same
> as any other software.

## Why this design

* **Works out of the box.** The high-level helpers (`slice()`, `query()`,
  `flowr_overview()`, `flowr_lint()`, `dataflow()`, `flowr_repl()`) open a shared
  session on first use. flowR's JS+wasm **bundle** ships inside the package, so
  on any machine with Node.js there is nothing to download or install.
* **Selectable engines.** flowR can be obtained as the shipped **bundle**
  (Node), a self-contained **binary** (no Node or Docker), a system/private
  **Node.js**, or **Docker** - chosen via `flowr_connect(engine=)` /
  `options(flowr.engine=)` / `FLOWR_ENGINE`.
* **Robust & low-maintenance.** A thin client over flowR's stable server
  protocol, with real timeouts, buffered message reassembly, streaming and
  strict resource cleanup. New flowR versions and query types work without
  updating this package.
* **Secure by default.** tree-sitter only (untrusted code never reaches an R
  interpreter), loopback-only server connections, and signature/checksum-verified
  binaries, with no shell invocation anywhere.
* **Fast.** One warm server is reused across calls; analyses are cached per
  content, so repeated operations avoid re-spawning and re-analysis.
* **Small.** Only `digest`, `jsonlite`, `openssl` and `sys` are required
  (`openssl`/`digest` verify downloaded binaries).

## AI-assisted development

This R adapter was developed with substantial assistance from an AI coding
assistant (Anthropic Claude), under human review. This disclosure covers **only
this adapter package** - in contrast, flowR itself (the analysis engine this
package talks to) is a separate, independently developed project and is **not**
AI-generated. Treat the adapter's generated code and docs accordingly, and
report anything amiss via `flowr::flowr_bug_report()`.

## Documentation

* `vignette("flowr")` - getting started (slicing, overview, linting).
* `vignette("flowr-engines")` - engines, the query API, the REPL, secure mode
  and performance.
* `vignette("flowr-security")` - binary integrity and how to turn on signature
  verification.

## Development

Dependencies are managed reproducibly with [rv](https://github.com/A2-ai/rv):

```sh
rv sync   # install the locked dependency set from CRAN
```

Regenerate documentation and run the checks:

```r
roxygen2::roxygenise()
# regression + functionality + leak tests
NOT_CRAN=true Rscript -e 'testthat::test_local()'
```

Targets flowR **2.11.1**.
