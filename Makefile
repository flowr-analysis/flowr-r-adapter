# Local development workflows for the flowr adapter.
# Dependencies are pinned with rv (rproject.toml / rv.lock); run `make sync` once.
# R is invoked with --no-init-file so dev tools resolve regardless of rv activation.

R := Rscript --no-init-file

.PHONY: help sync document test check hooks install reinstall reinstall_signed clean \
        sigdb binaries

# flowR version the release artefacts are built for; defaults to the package's
# own target so `make sigdb` needs no arguments. Override: make sigdb V=2.12.0
V := $(shell sed -n -E 's/.*flowr_version[[:space:]]*=[[:space:]]*"([^"]*)".*/\1/p' R/config.R | head -1)

# Run R with the rv-locked library on the search path (see tools/with-rvlib.sh).
WITH_RVLIB := tools/with-rvlib.sh

help:
	@echo "targets:"
	@echo "  sync      install the locked dependency set (rv)"
	@echo "  document  regenerate NAMESPACE and man/ from roxygen"
	@echo "  test      run the test suite (real-engine tests run when installed)"
	@echo "  check     R CMD check --as-cran on a fresh build"
	@echo "  hooks     install the local git pre-commit/pre-push guards"
	@echo "  install   document, then (re)install the package locally"
	@echo "  reinstall build a tarball and install it (with vignettes; no doc regen)"
	@echo "  reinstall_signed  sign the cached binary and reinstall to verify it"
	@echo "  sigdb     pack the signature-database release assets into out/ (V=<version>)"
	@echo "  binaries  compile the flowR binary for this machine into out/ (V=<version>)"
	@echo "  clean     remove build artefacts"

sync:
	rv sync

document:
	$(WITH_RVLIB) $(R) -e 'roxygen2::roxygenise()'

test:
	NOT_CRAN=true $(WITH_RVLIB) $(R) -e 'testthat::test_local(stop_on_failure = TRUE)'

check: document
	$(WITH_RVLIB) R --no-init-file CMD build .
	$(WITH_RVLIB) R --no-init-file CMD check --as-cran --no-manual $$(ls -t flowr_*.tar.gz | head -1)

# Point git at the tracked hook scripts. Run once per clone; re-running is safe.
hooks:
	git config core.hooksPath .githooks
	@echo "installed git hooks: pre-commit (tests), pre-push (R CMD check)."

install: document reinstall

# Build a source tarball first, then install THAT, so a local (re)install matches
# a real deploy: `R CMD build` renders the vignettes and stages help/data the way
# CRAN/GitHub installs do, so vignette("flowr") and ?flowr are present afterwards.
# Installing a source *directory* directly (install.packages(".") / R CMD INSTALL .)
# skips vignette building, which is why they were missing before.
reinstall:
	R --no-init-file CMD build .
	$(R) -e 'tb <- Sys.glob("flowr_*.tar.gz"); tb <- tb[order(file.mtime(tb), decreasing = TRUE)][1]; install.packages(tb, repos = NULL, type = "source")'

# Sign the locally cached binary with flowr-signing-key.pem, ship the matching
# public key, reinstall, and confirm flowr_status() reports signature-verified.
reinstall_signed: document
	openssl ec -in flowr-signing-key.pem -pubout -out inst/flowr-pubkey.pem
	cp inst/flowr-manifest.json inst/flowr-manifest.json.orig
	$(R) tools/sign_local.R
	$(MAKE) reinstall
	$(R) -e 'library(flowr); flowr_install(engine="binary", force=TRUE, quiet=TRUE); print(flowr_status()); stopifnot(identical(flowr:::.flowr_binary_verification(), "signature"))'
	mv inst/flowr-manifest.json.orig inst/flowr-manifest.json

# Build the release artefacts locally, with the same scripts the binaries
# workflow runs -- so what you can inspect here is what CI publishes. Both drop
# `<name>.tar.gz` + `.sha256` into out/; CI additionally signs them.
#   make sigdb                 # all three sets (base, current, history)
#   make sigdb SCOPES=current  # just one
sigdb:
	tools/pack-sigdb.sh $(V) out $(SCOPES)

# Only this machine's platform: cross-compiling all five is CI's job (and the
# Windows target must be built on Windows -- see .github/workflows/binaries.yaml).
binaries:
	tools/build-binaries.sh $(V) out

clean:
	rm -f flowr_*.tar.gz
	rm -rf flowr.Rcheck out .sigdb-work
