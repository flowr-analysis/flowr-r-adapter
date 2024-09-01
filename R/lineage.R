#' Sends a ["request-lineage"](https://github.com/flowr-analysis/flowr/wiki/Interface#the-lineage-request) request
#'
#' @param con The connection to the server
#' @param filetoken The filetoken retrieved by a previous "file-analysis"
#' request
#' @param criterion The criterion pointing to the object whose lineage should be
#' determined
#' @param id The id of the request
#' @return A list containing the id and the response
#'
#' @seealso [request_file_analysis()]
#' @seealso [connect()]
#'
#' @export
request_lineage <- function(con, filetoken, criterion, id = get_new_id()) {
  request <- list(
    type = "request-lineage",
    id = id,
    filetoken = filetoken,
    criterion = criterion
  )
  res <- send_request(con, request)

  if (res$type == "error") {
    return(list(error = res$reason))
  }

  return(list(id = id, res = res))
}
