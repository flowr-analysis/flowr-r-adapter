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
    r$buf <- raw(0)
    reg[[key]] <- r
  }
  r
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

.flowr_read_message <- function(con, reader, timeout) {
  deadline <- Sys.time() + timeout
  repeat {
    # scan only the bytes we have not scanned before, so accumulating a large
    # (multi-MB) response stays linear rather than re-scanning the whole buffer
    buf <- reader$buf
    scanned <- reader$scanned %||% 0L
    idx <- 0L
    if (length(buf) > scanned) {
      hit <- which(buf[(scanned + 1L):length(buf)] == as.raw(0x0a))
      if (length(hit) > 0) {
        idx <- scanned + hit[1L]
      }
    }
    if (idx > 0L) {
      line <- if (idx > 1L) buf[seq_len(idx - 1L)] else raw(0)
      if (length(line) > 0 && line[length(line)] == as.raw(0x0d)) {
        line <- line[-length(line)]
      }
      reader$buf <- if (idx < length(buf)) buf[-seq_len(idx)] else raw(0)
      reader$scanned <- 0L
      return(rawToChar(line))
    }
    reader$scanned <- length(buf)          # fully scanned, no newline yet
    remaining <- as.numeric(difftime(deadline, Sys.time(), units = "secs"))
    if (remaining <= 0) {
      stop("flowR request timed out after ", round(timeout), "s", call. = FALSE)
    }
    ready <- socketSelect(list(con), write = FALSE, timeout = remaining)
    if (!isTRUE(ready)) {
      next # timeout re-checked at top of loop
    }
    chunk <- readBin(con, what = "raw", n = 4194304L)
    if (length(chunk) == 0L) {
      # socket signalled readable but delivered nothing -> peer closed
      stop("flowR server closed the connection unexpectedly", call. = FALSE)
    }
    reader$buf <- c(buf, chunk)
  }
}

# Serialise and send a single message, then flush.
.flowr_write_message <- function(con, command) {
  json <- jsonlite::toJSON(command, auto_unbox = TRUE, null = "null", na = "null")
  writeBin(charToRaw(paste0(json, "\n")), con)
  flush(con)
  invisible(NULL)
}

.flowr_parse <- function(line) {
  jsonlite::fromJSON(
    line,
    simplifyVector = FALSE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE
  )
}

# Send a request and read its single response, raising flowR errors as R errors.
.flowr_request <- function(con, command, timeout = flowr_option("request_timeout")) {
  reader <- .flowr_reader_for(con)
  .flowr_log("-> ", command$type, if (!is.null(command$id)) paste0(" #", command$id) else "")
  .flowr_write_message(con, command)
  res <- .flowr_parse(.flowr_read_message(con, reader, timeout))
  .flowr_log("<- ", res$type %||% "?")
  if (identical(res$type, "error")) {
    stop("flowR error: ", res$reason %||% "unknown error", call. = FALSE)
  }
  res
}

# Send a request whose reply is streamed as several messages terminated by an
# `end-*` message (used by the REPL).  Returns the list of intermediate messages.
.flowr_request_stream <- function(con, command, end_type,
                                  timeout = flowr_option("request_timeout")) {
  reader <- .flowr_reader_for(con)
  .flowr_write_message(con, command)
  out <- list()
  repeat {
    msg <- .flowr_parse(.flowr_read_message(con, reader, timeout))
    if (identical(msg$type, "error")) {
      stop("flowR error: ", msg$reason %||% "unknown error", call. = FALSE)
    }
    if (identical(msg$type, end_type)) {
      break
    }
    out[[length(out) + 1L]] <- msg
  }
  out
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# Close a client connection and drop its read buffer.
.flowr_close <- function(con) {
  if (is.null(con)) {
    return(invisible(FALSE))
  }
  .flowr_reader_drop(con)
  close(con)
  invisible(TRUE)
}
