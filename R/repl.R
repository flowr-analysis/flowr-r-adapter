source("R/connect.R")
source("R/utils.R")

#' Sends a ["request-repl-execution"](https://github.com/flowr-analysis/flowr/wiki/Interface#the-repl-request) request
#'
#' @param con The connection to the server
#' @param expression The expression to be executed by the REPL
#' @param id The id of the request
#' @return A list containing the id and the response
#'
#' @seealso [connect()]
#'
#' @export
request_repl <- function(con, expression, id = get_new_id()) {
  request <- list(
    type = "request-repl-execution",
    id = id,
    expression = expression
  )
  res <- send_request(con, request)

  return(list(id = id, res = res))
}
