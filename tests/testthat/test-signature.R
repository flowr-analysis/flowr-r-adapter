# Signature verification must stay stable: the release signs with
# `openssl dgst -sha256 -sign` (ECDSA over SHA-256, the pinned key is EC P-256),
# and the R client verifies with openssl::signature_verify(hash = sha256). These
# tests lock that scheme in place and confirm the shipped public key is valid.

test_that("the pinned public key ships and parses", {
  skip_if_not_installed("openssl")
  pub <- system.file("flowr-pubkey.pem", package = "flowr")
  skip_if(!nzchar(pub), "no pinned public key in this build")
  expect_silent(key <- openssl::read_pubkey(pub))
  expect_s3_class(key, "pubkey")
})

test_that(".flowr_verify_sig accepts a valid ECDSA/SHA-256 signature", {
  skip_if_not_installed("openssl")
  key <- openssl::ec_keygen("P-256")               # same curve as the pinned key
  data <- as.raw(seq.int(0L, 255L))
  sig <- openssl::signature_create(data, hash = openssl::sha256, key = key)
  expect_true(flowr:::.flowr_verify_sig(data, sig, key$pubkey))
})

test_that("secure mode never skips signature verification (fails closed)", {
  withr::local_options(list(flowr.secure = TRUE))
  tf <- withr::local_tempfile(fileext = ".tar.gz"); writeLines("x", tf)
  # no signature URL, and an unreachable signature, must both stop - not skip
  expect_error(flowr:::.flowr_verify_signature(tf, NULL), "secure mode")
  expect_error(flowr:::.flowr_verify_signature(tf, "file:///no/such.sig"), "secure mode")
})

test_that("outside secure mode a missing signature degrades to checksum-only (with a warning)", {
  withr::local_options(list(flowr.secure = FALSE, flowr.verify_signature = TRUE))
  tf <- withr::local_tempfile(fileext = ".tar.gz"); writeLines("x", tf)
  expect_warning(res <- flowr:::.flowr_verify_signature(tf, NULL), "checksum-only|signature")
  expect_true(is.na(res))
})

test_that(".flowr_verify_sig rejects tampering and wrong keys without erroring", {
  skip_if_not_installed("openssl")
  key <- openssl::ec_keygen("P-256")
  data <- as.raw(seq.int(0L, 255L))
  sig <- openssl::signature_create(data, hash = openssl::sha256, key = key)

  # a single flipped byte must fail verification
  tampered <- data
  tampered[1] <- as.raw(bitwXor(as.integer(tampered[1]), 1L))
  expect_false(flowr:::.flowr_verify_sig(tampered, sig, key$pubkey))

  # a signature made with a different key must fail (not crash)
  other <- openssl::ec_keygen("P-256")
  expect_false(flowr:::.flowr_verify_sig(data, sig, other$pubkey))

  # garbage bytes as a "signature" are rejected, not thrown
  expect_false(flowr:::.flowr_verify_sig(data, as.raw(1:16), key$pubkey))
})
