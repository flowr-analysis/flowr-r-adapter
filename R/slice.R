#' Sends a ["request-slice"](https://github.com/flowr-analysis/flowr/wiki/Interface#the-slice-request) request
#'
#' @param con The connection to the server
#' @param filetoken The filetoken of the file to be sliced
#' (retrieved by a previous "file-analysis" request)
#' @param criteria The slice's criteria
#' @param id The id of the request
#' @return A list containing the id and the response
#'
#' @seealso [request_file_analysis()]
#' @seealso [connect()]
#'
#' @export
request_slice <- function(con, filetoken, criteria, id = get_new_id()) {
  request <- list(
    type = "request-slice",
    id = id,
    filetoken = filetoken,
    criterion = I(criteria)
  )
  res <- send_request(con, request)

  if (handle_err_result(res)) {
    return(NULL)
  }

  return(list(id = id, res = res))
}
