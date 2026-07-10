# Maintaining flowr - the important things

A short checklist for releasing and keeping this package working. Everyday dev
commands are in the `Makefile` (`make document`, `make test`, `make check`).

## 1. Enable signed binaries (one-time)

Generate a signing keypair, commit the **public** key, keep the **private** key
as a GitHub secret. The R client pins the public key, so only binaries signed
with the matching private key verify.

```sh
openssl ecparam -name prime256v1 -genkey -noout -out flowr-signing-key.pem
openssl ec -in flowr-signing-key.pem -pubout -out inst/flowr-pubkey.pem   # commit this
# Repo -> Settings -> Secrets -> Actions: add FLOWR_SIGNING_KEY = contents of flowr-signing-key.pem
```

Keep `flowr-signing-key.pem` private (never commit it). Without a shipped
`inst/flowr-pubkey.pem`, binaries are still SHA-256-verified but not signature-checked.
That is why a local build shows `binary: checksum only (no signature)` in
`flowr_status()` - no public key is pinned yet.

### Test signed binaries locally

To exercise the full signature path on your machine (and see `flowr_status()`
turn green), sign a binary with your private key and point flowr at the local
files via a manifest:

```sh
# 1. ship the public key and reinstall so it is pinned
openssl ec -in flowr-signing-key.pem -pubout -out inst/flowr-pubkey.pem
make reinstall

# 2. sign a built archive (same scheme as CI)
openssl dgst -sha256 -sign flowr-signing-key.pem \
  -out flowr-2.11.1-linux-x64.tar.gz.sig  flowr-2.11.1-linux-x64.tar.gz

# 3. point the R client at the local files (URLs may be file://), e.g. a manifest
cat > inst/flowr-manifest.json <<'JSON'
{ "binaries": [ {
  "version": "2.11.1", "platform": "linux-x64",
  "url":    "file:///abs/path/flowr-2.11.1-linux-x64.tar.gz",
  "sha256": "<shasum -a 256 of the tar.gz>",
  "sig":    "file:///abs/path/flowr-2.11.1-linux-x64.tar.gz.sig"
} ] }
JSON
make reinstall
```

Then `flowr_install(engine = "binary", force = TRUE)` verifies the signature and
`flowr_status()` shows `binary: signature-verified` (green). A tampered archive
or wrong key aborts the install.

## 2. Publish the native binaries for a flowR version

Run the **build-flowr-binaries** GitHub Action (Actions tab -> Run workflow),
entering the flowR version (default `2.11.1`). It compiles a self-contained
binary per platform (linux/darwin x64+arm64, win x64), uploads each as
`flowr-<version>-<os>-<arch>.tar.gz` plus a `.sha256` and (if the secret is set)
a `.sig`, to the `flowr-v<version>` release. The R side downloads and verifies
these on demand.

## 3. Update the shipped bundle when bumping flowR

The universal fallback in `inst/flowr-js/` (works everywhere with Node) must be
rebuilt when you change the target flowR version:

```sh
npm install @eagleoutice/flowr@<version> ignore esbuild
npx esbuild --bundle node_modules/@eagleoutice/flowr/cli/flowr.js \
  --platform=node --minify --target=node18 --outfile=inst/flowr-js/flowr.min.js
cp node_modules/@davisvaughan/tree-sitter-r/tree-sitter-r.wasm inst/flowr-js/
cp node_modules/web-tree-sitter/tree-sitter.wasm inst/flowr-js/
echo "VERSION <version>" > inst/flowr-js/VERSION
```

Then bump the default in `R/config.R` (`flowr_version`) and re-run `make check`.

## 4. Dependencies

Locked with [rv](https://github.com/A2-ai/rv): `make sync` (or `rv sync`) installs
the exact set from CRAN. Update with `rv add <pkg>` / `rv upgrade`.

## 5. Cutting a release

Releases are automated the same way as the VS Code extension: start a commit
message with `[release:patch]`, `[release:minor]` or `[release:major]`. On push
to `main` the **release** workflow bumps `Version:` in `DESCRIPTION` accordingly,
commits `Release X.Y.Z`, tags `vX.Y.Z`, and creates a GitHub release with
generated notes. Any other commit is a no-op, and the bump commit does not
re-trigger the workflow.

Before a release commit: `make document && make test && make check` (checks must
be WARNING/ERROR-free; the `inst/flowr-js` size NOTE and the "New submission"
NOTE are expected) and update `NEWS.md`.

Note: the CRAN package name `flowr` is currently held by an archived, unrelated
package - resolve the name (reclaim or rename) before a CRAN submission.
