source("R/connect.R")
source("R/utils.R")

#' Sends a ["request-repl-execution"](https://github.com/flowr-analysis/flowr/wiki/Interface#the-repl-request) request
#'
#' @param expression The expression to be executed by the REPL
#' @param id The id of the request
#' @param con The connection to the server
#' @return A list containing the id and the response
#'
#' @seealso [connect()]
#'
#' @export
request_repl <- function(expression, id = get_new_id(), con) {
  request <- fromRJSON('{
    "type":       "request-repl-execution",
    "id":         "%s",
    "expression": "%s",
  }', id, expression)
  res <- send_request(con, request)

  return(list(id = id, res = res))
}
