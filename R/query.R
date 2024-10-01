#' Sends a ["request-query"](https://github.com/flowr-analysis/flowr/wiki/Interface#the-query-request) request
#'
#' @param con The connection to the server
#' @param filetoken The filetoken of the file to be sliced
#' (retrieved by a previous "file-analysis" request)
#' @param query The query to be executed
#' @param id The id of the request
#' @return A list containing the id and the response
#'
#' @seealso [request_file_analysis()]
#' @seealso [connect()]
#'
#' @export
request_query <- function(con, filetoken, query, id = get_new_id()) {
  request <- list(
    type = "request-query",
    id = id,
    filetoken = filetoken,
    query = I(query)
  )
  return(send_request_handle_response(con, request))
}
