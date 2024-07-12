# TODO should this really be a global scope, or should the connection be returned/passed every time in these functions?
connection <<- NULL

#' Connects to a running flowR server at the given host and port
#'
#' @seealso [send_flowr_request()]
#' @seealso [disconnect_flowr()]
#'
#' @export
connect_flowr_if_necessary <- function(host = "localhost", port = 1042, blocking = TRUE) {
  if (!is.null(connection)) {
    return(list("connection" = connection))
  }

  connection <<- socketConnection(host = host, port = port, server = FALSE, blocking = blocking, open = "r+")

  # the first response is the hello message
  hello <- readLines(connection, n = 1)
  return(list("connection" = connection, "hello" = hello))
}

#' Sends a JSON request to the running flowR server, if connected
#'
#' @seealso [connect_flowr_if_necessary()]
#' @seealso [disconnect_flowr()]
#'
#' @export
send_flowr_request <- function(command) {
  if (is.null(connection)) {
    return()
  }

  request <- jsonlite::toJSON(command, auto_unbox = TRUE)
  writeLines(request, connection)

  response <- readLines(connection, n = 1)
  # we don't simplify anything, so that everything is always a predictable list of lists!
  return(jsonlite::fromJSON(response, simplifyVector = FALSE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE))
}

#' Closes an active connection to a flowR server, if connected
#'
#' @seealso [connect_flowr_if_necessary()]
#' @seealso [send_flowr_request()]
#'
#' @export
disconnect_flowr <- function() {
  if (is.null(connection)) {
    return(FALSE)
  }

  close(connection)
  connection <<- NULL
  return(TRUE)
}
