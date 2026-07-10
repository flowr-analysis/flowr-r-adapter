# Startup banner, one-time hints, and the progress line / notice interaction.
#
# Regression guard: the transient "[flowr] starting the flowR engine ..."
# progress line must never be glued onto a message emitted while the engine is
# starting (e.g. the "no downloaded binary" notice). A notice has to end the
# open progress line first, so each lands on its own clean line.

# The package's live notification state (an environment; reference semantics, so
# writing through this local handle mutates the real state). Bound via a local
# because `flowr:::.flowr_state$x <- v` is a complex assignment R cannot route
# through `:::` ("object 'flowr' not found").
notify_state <- function() flowr:::.flowr_state

# Fresh, isolated notification state for a test.
reset_notify <- function() {
  st <- notify_state()
  st$msg_counts <- NULL
  st$progress_active <- NULL
}

test_that("a notice ends an open progress line before printing (no glued output)", {
  withr::local_options(list(flowr.quiet = FALSE, flowr.message_limit = 3L,
                            flowr.color = FALSE))
  reset_notify()
  # simulate an on-screen, unterminated progress line
  st <- notify_state()
  st$progress_active <- TRUE

  out <- capture.output(
    expect_message(flowr:::.flowr_notify("no-binary", "using the bundled engine"),
                   "using the bundled engine"),
    type = "output")
  # the transient line was erased (carriage return + clear-to-end-of-line) ...
  expect_true(grepl("\r\033\\[K", paste0(out, collapse = "")))
  # ... and marked closed, so the later on.exit clear becomes a no-op
  expect_null(notify_state()$progress_active)
})

test_that("clearing a progress line is idempotent", {
  st <- notify_state()
  st$progress_active <- TRUE
  first <- capture.output(flowr:::.flowr_progress_clear(TRUE), type = "output")
  expect_true(grepl("\033\\[K", paste0(first, collapse = "")))
  # a second clear must not emit anything (the line is already gone)
  second <- capture.output(flowr:::.flowr_progress_clear(TRUE), type = "output")
  expect_identical(paste0(second, collapse = ""), "")
})

test_that("a limit-1 hint shows only on the first call", {
  withr::local_options(list(flowr.quiet = FALSE, flowr.message_limit = 3L))
  reset_notify()
  expect_message(flowr:::.flowr_notify("startup-banner", limit = 1L, "see docs"),
                 "see docs")
  expect_silent(flowr:::.flowr_notify("startup-banner", limit = 1L, "see docs"))
})

test_that("quiet mode silences notices entirely", {
  withr::local_options(list(flowr.quiet = TRUE))
  reset_notify()
  expect_silent(flowr:::.flowr_notify("startup-banner", "flowr loaded"))
})

# Capture the packageStartupMessage() lines emitted by .onAttach().
attach_lines <- function() {
  msgs <- character(0)
  withCallingHandlers(
    flowr:::.onAttach("lib", "flowr"),
    packageStartupMessage = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    })
  # split multi-line messages so each logical line can be asserted on
  unlist(strsplit(msgs, "\n", fixed = TRUE))
}

test_that("the on-load banner is one short line with the guide, shown only once", {
  withr::local_options(list(flowr.quiet = FALSE, flowr.message_limit = 3L,
                            flowr.color = FALSE))
  reset_notify()

  first <- attach_lines()
  # select the banner precisely: "flowr <version> loaded. ..." (not the "no
  # downloaded binary" notice, which also contains the substring "loaded")
  banner <- grep("^flowr [0-9].*loaded", first, value = TRUE)
  expect_length(banner, 1L)
  # the guide sits on the SAME line as the banner
  expect_match(banner, "Getting started: (vignette\\(\"flowr\"\\)|\\?flowr)")

  # a second attach in the same session must not repeat the banner (limit 1)
  second <- attach_lines()
  expect_false(any(grepl("^flowr [0-9].*loaded", second)))
})
