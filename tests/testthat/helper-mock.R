# A minimal in-R flowR-compatible socket server used to exercise the real
# protocol client without Node/Docker/network. It speaks the same
# newline-delimited JSON protocol and returns canned, correctly shaped
# responses, so the client's framing, streaming, error handling and resource
# cleanup can be tested deterministically.

.mock_server_program <- '
local({
  port <- as.integer(commandArgs(trailingOnly = TRUE)[[1]])
  send <- function(con, obj) {
    writeBin(charToRaw(paste0(jsonlite::toJSON(obj, auto_unbox = TRUE, null = "null"), "\\n")), con)
    flush(con)
  }
  # This server connection is blocking, so a large readBin(n) would block until
  # n bytes arrive; read one byte at a time (after socketSelect signals data)
  # to assemble a line without over-reading.
  read_line <- function(con) {
    buf <- raw(0)
    repeat {
      if (!isTRUE(socketSelect(list(con), timeout = 30))) return(NULL)
      b <- readBin(con, "raw", 1L)
      if (length(b) == 0) return(NULL)
      if (b == as.raw(0x0a)) return(rawToChar(buf))
      buf <- c(buf, b)
    }
  }
  serve <- function(con) {
    send(con, list(type = "hello", clientName = "mock",
                   versions = list(flowr = "mock", r = "n/a", engine = "tree-sitter")))
    repeat {
      line <- read_line(con)
      if (is.null(line)) break
      req <- jsonlite::fromJSON(line, simplifyVector = FALSE)
      id <- req$id
      if (identical(req$type, "request-file-analysis")) {
        if (identical(req$content, "ERR")) { send(con, list(type = "error", id = id, reason = "boom")); next }
        send(con, list(type = "response-file-analysis", id = id,
                       results = list(normalize = list(ast = list()))))
      } else if (identical(req$type, "request-query")) {
        qtypes <- vapply(req$query, function(q) q$type, character(1))
        if ("static-slice" %in% qtypes) {
          send(con, list(type = "response-query", id = id, results = list(
            "static-slice" = list(.meta = list(), results = list(
              q = list(slice = list(result = list(1, 2)),
                       reconstruct = list(code = "x <- 1")))))))
        } else if ("signature" %in% qtypes) {
          q <- req$query[[which(qtypes == "signature")[1]]]
          if (identical(q$package, "*") && identical(q[["function"]], "mockfn")) {
            send(con, list(type = "response-query", id = id, results = list(signature = list(
              .meta = list(), databases = list(list(scope = "current", version = 1, date = "2026-01-01")),
              packageCount = 1L, sourceCount = 1L,
              matches = list(list(package = "mockpkg", name = "mockfn", exported = TRUE, version = "1.0.0"))))))
          } else if (identical(q$package, "mockpkg") && identical(q[["function"]], "mockfn")) {
            send(con, list(type = "response-query", id = id, results = list(signature = list(
              .meta = list(), databases = list(list(scope = "current", version = 1, date = "2026-01-01")),
              packageCount = 1L, sourceCount = 1L,
              "function" = list(name = "mockfn", package = "mockpkg", version = "1.0.0",
                                exported = TRUE, properties = list(),
                                parameters = list(list(name = "x", required = TRUE, forced = FALSE)),
                                callees = list())))))
          } else if (identical(q$package, "totally_unknown_pkg")) {
            # mirrors real flowR: message + packageCount, no `package` field
            send(con, list(type = "response-query", id = id, results = list(signature = list(
              .meta = list(), databases = list(list(scope = "current", version = 1, date = "2026-01-01")),
              packageCount = 23765L, sourceCount = 1L,
              message = "The signature database does not know the package \\"totally_unknown_pkg\\".",
              suggestions = list()))))
          } else {
            send(con, list(type = "response-query", id = id, results = list(signature = list(
              .meta = list(), databases = list(), packageCount = 0L, sourceCount = 0L))))
          }
        } else if ("guess-dep-versions" %in% qtypes) {
          send(con, list(type = "response-query", id = id, results = list(
            "guess-dep-versions" = list(
              .meta = list(), rVersion = "4.5.0", runnableCombinations = 1, possibleCombinations = 1,
              dependencies = list(list(
                package = "mockpkg", base = FALSE, used = TRUE, range = "1.0.0",
                minVersion = "1.0.0", maxVersion = "1.0.0", candidateCount = 1L, totalVersions = 1L,
                candidates = list("1.0.0"),
                evidence = list(list(source = "available", origin = "signature database",
                                     detail = "data available from 1.0.0", bound = ">=1.0.0"))))))))
        } else {
          send(con, list(type = "response-query", id = id, results = list(
            dependencies = list(
              library = list(list(nodeId = 1, functionName = "library", value = "dplyr")),
              source = list(), read = list(), write = list()))))
        }
      } else if (identical(req$type, "request-repl-execution")) {
        send(con, list(type = "response-repl-execution", id = id, stream = "stdout", result = "hello "))
        send(con, list(type = "response-repl-execution", id = id, stream = "stdout", result = "world"))
        send(con, list(type = "end-repl-execution", id = id))
      } else {
        send(con, list(type = "error", id = id, reason = paste("unknown", req$type)))
      }
    }
  }
  repeat {
    con <- tryCatch(socketConnection(host = "127.0.0.1", port = port, server = TRUE,
                                     blocking = TRUE, open = "r+b", timeout = 120),
                    error = function(e) NULL)
    if (is.null(con)) { Sys.sleep(0.1); next }
    tryCatch(serve(con), error = function(e) NULL)
    close(con)
  }
})
'

# Start the mock server in a background R process; returns list(port, pid).
mock_server_start <- function() {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("jsonlite")
  rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  port <- flowr:::.flowr_free_port()
  script <- tempfile(fileext = ".R")
  writeLines(.mock_server_program, script)
  pid <- sys::exec_background(rscript, c(script, as.character(port)),
                              std_out = tempfile(), std_err = tempfile())
  ready <- FALSE
  for (i in seq_len(80)) {
    # Poll until the server is up; suppress the expected "port cannot be opened"
    # warning each probe emits before it starts (a real failure hits skip() below).
    con <- suppressWarnings(tryCatch(
      socketConnection("127.0.0.1", port, server = FALSE, blocking = FALSE,
                       open = "r+b", timeout = 1),
      error = function(e) NULL
    ))
    if (!is.null(con)) { close(con); ready <- TRUE; break }
    Sys.sleep(0.1)
  }
  if (!ready) {
    tryCatch(tools::pskill(pid), error = function(e) NULL)
    testthat::skip("could not start mock flowR server")
  }
  Sys.sleep(0.2)
  list(port = port, pid = pid)
}

mock_server_stop <- function(srv) {
  tryCatch(tools::pskill(srv$pid), error = function(e) NULL)
}
