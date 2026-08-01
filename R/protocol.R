# Wire protocol ---------------------------------------------------------------
#
# flowR's server speaks newline-delimited JSON: every message is a single JSON
# object terminated by "\n".  The tricky part on the R side is reading a
# *complete* message without (a) blocking forever on a dead server or (b)
# mistaking a partially delivered TCP chunk for a whole line.  We therefore keep
# a per-connection byte buffer, read with a non-blocking socket, and use
# `socketSelect()` to enforce a wall-clock timeout.  This replaces the previous
# single `readLines(n = 1)` which had no timeout, no EOF detection, and could be
# fooled by partial reads.

# Registry mapping a connection's number to its read buffer environment, so the
# low-level (connection-only) API can stay backwards compatible.
.flowr_reader_registry <- function() {
  if (is.null(.flowr_state$readers)) {
    .flowr_state$readers <- new.env(parent = emptyenv())
  }
  .flowr_state$readers
}

.flowr_reader_for <- function(con) {
  reg <- .flowr_reader_registry()
  key <- as.character(as.integer(con))
  r <- reg[[key]]
  if (is.null(r)) {
    r <- new.env(parent = emptyenv())
    reg[[key]] <- r
  }
  .flowr_reader_init(r)
}

# A reader holds
#   buf/off        the contiguous bytes we have already joined, and how many of
#                  them are consumed (an offset instead of trimming the front,
#                  so draining many messages out of one buffer stays linear),
#   nl/nl_i        the newline positions inside `buf` and the next unconsumed
#                  one (found once, never re-scanned),
#   pending/…      raw chunks straight off the socket that have not been joined
#                  yet, with the newline offsets found in each as it arrived.
# Everything here exists so that reading an N-byte reply costs O(N): the old
# reader re-concatenated and re-scanned the whole buffer on every socket read,
# which is quadratic and falls over on the tens of megabytes a real project's
# analysis produces.
.flowr_reader_init <- function(reader) {
  if (is.null(reader$nl)) {
    b <- reader$buf %||% raw(0)
    reader$buf <- b
    reader$off <- 0L
    reader$nl <- which(b == as.raw(0x0a))
    reader$nl_i <- 1L
    reader$pending <- list()
    reader$pending_nl <- list()
    reader$has_nl <- FALSE
  }
  reader
}

.flowr_reader_drop <- function(con) {
  reg <- .flowr_reader_registry()
  rm(list = as.character(as.integer(con)), envir = reg)
}

# Open a non-blocking client socket to a flowR server.
.flowr_open_socket <- function(host, port, timeout = 10) {
  socketConnection(
    host = host, port = port, server = FALSE,
    blocking = FALSE, open = "r+b", timeout = timeout
  )
}

# Read exactly one newline-delimited message, honouring `timeout` seconds.
# Returns the message as a string (without the trailing newline).  Errors on
# timeout or if the server closes the connection mid-message.
# Pure helper: pull the first newline-terminated line out of a raw buffer.
# Returns NULL if no complete line yet, else list(line = <string>, rest = <raw>).
# CRLF is tolerated. Kept separate so the framing logic can be unit-tested
# without a live socket.
.flowr_extract_line <- function(buf) {
  idx <- which(buf == as.raw(0x0a))
  if (length(idx) == 0) {
    return(NULL)
  }
  end <- idx[1]
  line <- if (end > 1L) buf[seq_len(end - 1L)] else raw(0)
  if (length(line) > 0 && line[length(line)] == as.raw(0x0d)) {
    line <- line[-length(line)]
  }
  list(line = rawToChar(line),
       rest = if (end < length(buf)) buf[-seq_len(end)] else raw(0))
}

# How much we take off the socket in one go.
.flowr_read_chunk_size <- 4194304L

# Fold the pending chunks into the contiguous buffer, dropping the already
# consumed prefix. Called only once a newline is known to be somewhere in the
# pending chunks, so the buffer is rebuilt once per *message*, not once per read.
.flowr_reader_compact <- function(reader) {
  buf <- reader$buf
  off <- reader$off
  kept <- if (off < length(buf)) buf[(off + 1L):length(buf)] else raw(0)
  chunks <- reader$pending
  # `kept` holds no unconsumed newline (we only get here once nl is exhausted),
  # so the joined newline positions are just the per-chunk ones, shifted
  nl <- vector("list", length(chunks))
  base <- length(kept)
  for (i in seq_along(chunks)) {
    p <- reader$pending_nl[[i]]
    if (length(p) > 0) {
      nl[[i]] <- p + base
    }
    base <- base + length(chunks[[i]])
  }
  reader$buf <- unlist(c(list(kept), chunks), use.names = FALSE)
  reader$off <- 0L
  reader$nl <- unlist(nl, use.names = FALSE) %||% integer(0)
  reader$nl_i <- 1L
  reader$pending <- list()
  reader$pending_nl <- list()
  reader$has_nl <- FALSE
  invisible(NULL)
}

# R caps a single string at 2^31-1 bytes; say so plainly instead of letting
# rawToChar() fail with "long vectors not supported".
.flowr_raw_to_string <- function(bytes) {
  if (length(bytes) >= 2147483647) {
    .flowr_stop("flowR sent a ", round(length(bytes) / 1048576),
                " MB message, which is past R's 2 GB limit for a single string; ",
                "analyse fewer files per call")
  }
  rawToChar(bytes)
}

# Pop the next complete message out of the reader, or NULL if we need more bytes.
.flowr_reader_take <- function(reader) {
  if (reader$nl_i > length(reader$nl)) {
    if (!isTRUE(reader$has_nl)) {
      return(NULL)
    }
    .flowr_reader_compact(reader)
    if (reader$nl_i > length(reader$nl)) {
      return(NULL)
    }
  }
  end <- reader$nl[[reader$nl_i]]
  reader$nl_i <- reader$nl_i + 1L
  line <- if (end > reader$off + 1L) reader$buf[(reader$off + 1L):(end - 1L)] else raw(0)
  reader$off <- end
  if (length(line) > 0 && line[[length(line)]] == as.raw(0x0d)) {
    line <- line[-length(line)]
  }
  # release a fully drained buffer at once, so a multi-MB reply does not stay
  # resident for the rest of the session
  if (reader$nl_i > length(reader$nl) && reader$off >= length(reader$buf)) {
    reader$buf <- raw(0)
    reader$off <- 0L
    reader$nl <- integer(0)
    reader$nl_i <- 1L
  }
  .flowr_raw_to_string(line)
}

# `timeout` is a *no-progress* timeout: it bounds how long we wait without any
# new bytes, not how long the whole transfer may take. Streaming a 200 MB
# analysis over a slow link therefore succeeds, while a dead or wedged server is
# still caught after `timeout` seconds of silence.
.flowr_read_message <- function(con, reader, timeout) {
  .flowr_reader_init(reader)
  deadline <- Sys.time() + timeout
  repeat {
    line <- .flowr_reader_take(reader)
    if (!is.null(line)) {
      return(line)
    }
    remaining <- as.numeric(difftime(deadline, Sys.time(), units = "secs"))
    if (remaining <= 0) {
      .flowr_stop("flowR sent nothing for ", round(timeout), "s. If the input is ",
                  "large, raise the limit with ",
                  "options(flowr.request_timeout = <seconds>)")
    }
    ready <- socketSelect(list(con), write = FALSE, timeout = remaining)
    if (!isTRUE(ready)) {
      next # timeout re-checked at top of loop
    }
    chunk <- readBin(con, what = "raw", n = .flowr_read_chunk_size)
    if (length(chunk) == 0L) {
      # socket signalled readable but delivered nothing -> peer closed
      .flowr_stop("flowR server closed the connection unexpectedly")
    }
    i <- length(reader$pending) + 1L
    reader$pending[[i]] <- chunk
    pos <- which(chunk == as.raw(0x0a))
    reader$pending_nl[[i]] <- pos
    if (length(pos) > 0) {
      reader$has_nl <- TRUE
    }
    deadline <- Sys.time() + timeout      # progress: restart the idle timeout
  }
}

# Serialise and send a single message, then flush. Written in bounded slices:
# inlining a whole project's sources produces requests of many megabytes, and
# handing that to writeBin() in one call needlessly doubles peak memory.
.flowr_write_message <- function(con, command) {
  json <- jsonlite::toJSON(command, auto_unbox = TRUE, null = "null", na = "null")
  bytes <- charToRaw(as.character(json))
  n <- length(bytes)
  from <- 1L
  while (from <= n) {
    to <- min(n, from + .flowr_read_chunk_size - 1L)
    writeBin(bytes[from:to], con)
    from <- to + 1L
  }
  writeBin(as.raw(0x0a), con)
  flush(con)
  invisible(NULL)
}

# Chunked replies -------------------------------------------------------------
#
# A reply too large for `JSON.stringify` (V8 caps a single string, and a
# project-sized analysis is past that cap) is streamed by flowR in chunks
# instead, from `bigStringify` in its util/json.js.  The chunks arrive
# back-to-back and are reassembled here into the one newline-terminated message
# they form -- that part needs nothing special.
#
# What the chunked writer does differently is escaping.  Where `JSON.stringify`
# runs values through a replacer, the chunk writer interpolates two kinds of
# value verbatim:
#
#   * the built-in-environment placeholder, as bare `"parent":<BuiltInEnvironment>`
#     rather than the string `"<BuiltInEnvironment>"` the replacer produces, and
#   * `RegExp` values, as `"${re.toString()}"`, so a pattern's own backslashes
#     (`dev\.new`) arrive unescaped.
#
# Neither survives a strict JSON parser, so we normalise both back to the form
# the non-chunked path emits before handing the message to jsonlite.  Both
# rewrites are exact -- there is a single well-defined form each value should
# have had -- and they run only after a parse has already failed, so a reply
# that is already valid JSON is never touched.
.flowr_repair_json <- function(line) {
  # A placeholder in value position becomes the string it stands for.  Anchoring
  # on both sides (a value follows `,` `:` or `[` and is followed by `,` `}` or
  # `]`) keeps the rewrite off anything inside a string literal.
  line <- gsub("(?<=[,:[])(<[A-Za-z]+>)(?=[,}\\]])", "\"\\1\"", line, perl = TRUE)
  # A backslash that opens an escape JSON does not define gets escaped itself.
  # `((?:\\\\)*)` consumes complete backslash pairs first, so a legitimately
  # escaped `\\` is never mistaken for the start of a bad escape and the
  # rewrite is idempotent.
  gsub("(?<!\\\\)((?:\\\\\\\\)*)\\\\([^\"\\\\/bfnrtu])", "\\1\\\\\\\\\\2",
       line, perl = TRUE)
}

.flowr_parse <- function(line) {
  one <- function(x) {
    jsonlite::fromJSON(
      x,
      simplifyVector = FALSE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE
    )
  }
  tryCatch(one(line), error = function(e) {
    repaired <- .flowr_repair_json(line)
    if (!identical(repaired, line)) {
      out <- tryCatch(one(repaired), error = function(e2) NULL)
      if (!is.null(out)) {
        .flowr_log("normalised escaping of a chunk-serialised reply")
        return(out)
      }
    }
    .flowr_stop("could not parse flowR's ",
                sprintf("%.1f MB", nchar(line, type = "bytes") / 1048576),
                " reply: ", conditionMessage(e))
  })
}

# The idle timeout to use for the input currently under analysis. flowR's work
# grows with the amount of source it was given, so a fixed 120s that is generous
# for a script is far too tight for a package with thousands of files: we add
# `timeout_per_mb` seconds for every megabyte of input. `flowr_analyze()` records
# the size of the last input it sent; queries against that same filetoken then
# inherit the same allowance.
.flowr_scaled_timeout <- function(bytes = .flowr_state$input_bytes %||% 0) {
  flowr_option("request_timeout") +
    (as.numeric(bytes) / 1048576) * flowr_option("timeout_per_mb")
}

# Send a request and read its single response, raising flowR errors as R errors.
.flowr_request <- function(con, command, timeout = .flowr_scaled_timeout()) {
  reader <- .flowr_reader_for(con)
  .flowr_log("-> ", command$type, if (!is.null(command$id)) paste0(" #", command$id) else "")
  .flowr_write_message(con, command)
  res <- .flowr_parse(.flowr_read_message(con, reader, timeout))
  .flowr_log("<- ", res$type %||% "?")
  if (identical(res$type, "error")) {
    .flowr_stop("flowR error: ", res$reason %||% "unknown error")
  }
  res
}

# Send a request whose reply is streamed as several messages terminated by an
# `end-*` message (used by the REPL).  Returns the list of intermediate messages.
.flowr_request_stream <- function(con, command, end_type,
                                  timeout = .flowr_scaled_timeout()) {
  reader <- .flowr_reader_for(con)
  .flowr_write_message(con, command)
  out <- list()
  repeat {
    msg <- .flowr_parse(.flowr_read_message(con, reader, timeout))
    if (identical(msg$type, "error")) {
      .flowr_stop("flowR error: ", msg$reason %||% "unknown error")
    }
    if (identical(msg$type, end_type)) {
      break
    }
    out[[length(out) + 1L]] <- msg
  }
  out
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# The package's single error entry point. Assembles its arguments into one
# message like base `stop()` does, always raises without the (noisy, internal)
# call, and tags the condition with class `flowr_error` so callers can catch
# flowr's own errors specifically. Every user-facing error goes through this, so
# they all read the same way. Use base `stop(cond)` only to re-raise an existing
# condition unchanged.
.flowr_stop <- function(..., class = NULL) {
  cond <- structure(
    class = c(class, "flowr_error", "error", "condition"),
    list(message = .makeMessage(...), call = NULL)
  )
  stop(cond)
}

# Warning counterpart of `.flowr_stop()`: same house style, no internal call,
# tagged `flowr_warning`. Every user-facing warning goes through this.
.flowr_warn <- function(..., class = NULL) {
  cond <- structure(
    class = c(class, "flowr_warning", "warning", "condition"),
    list(message = .makeMessage(...), call = NULL)
  )
  warning(cond)
}

# Close a client connection and drop its read buffer.
.flowr_close <- function(con) {
  if (is.null(con)) {
    return(invisible(FALSE))
  }
  .flowr_reader_drop(con)
  close(con)
  invisible(TRUE)
}
