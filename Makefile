# Local development workflows for the flowr adapter.
# Dependencies are pinned with rv (rproject.toml / rv.lock); run `make sync` once.
# R is invoked with --no-init-file so dev tools resolve regardless of rv activation.

R := Rscript --no-init-file

.PHONY: help sync document test check install reinstall reinstall_signed clean

help:
	@echo "targets:"
	@echo "  sync      install the locked dependency set (rv)"
	@echo "  document  regenerate NAMESPACE and man/ from roxygen"
	@echo "  test      run the test suite (real-engine tests run when installed)"
	@echo "  check     R CMD check --as-cran on a fresh build"
	@echo "  install   document, then (re)install the package locally"
	@echo "  reinstall install the package locally without regenerating docs"
	@echo "  reinstall_signed  sign the cached binary and reinstall to verify it"
	@echo "  clean     remove build artefacts"

sync:
	rv sync

document:
	$(R) -e 'roxygen2::roxygenise()'

test:
	NOT_CRAN=true $(R) -e 'testthat::test_local()'

check: document
	R --no-init-file CMD build .
	R --no-init-file CMD check --as-cran --no-manual $$(ls -t flowr_*.tar.gz | head -1)

# install.packages(type = "source") is used instead of `R CMD INSTALL` so a
# single command works everywhere (no shell activation, no sandbox surprises).
install: document reinstall

reinstall:
	$(R) -e 'install.packages(".", repos = NULL, type = "source")'

# Sign the locally cached binary with flowr-signing-key.pem, ship the matching
# public key, reinstall, and confirm flowr_status() reports signature-verified.
reinstall_signed: document
	openssl ec -in flowr-signing-key.pem -pubout -out inst/flowr-pubkey.pem
	cp inst/flowr-manifest.json inst/flowr-manifest.json.orig
	$(R) tools/sign_local.R
	$(MAKE) reinstall
	$(R) -e 'library(flowr); flowr_install(engine="binary", force=TRUE, quiet=TRUE); print(flowr_status()); stopifnot(identical(flowr:::.flowr_binary_verification(), "signature"))'
	mv inst/flowr-manifest.json.orig inst/flowr-manifest.json

clean:
	rm -f flowr_*.tar.gz
	rm -rf flowr.Rcheck
