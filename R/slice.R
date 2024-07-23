source("R/connect.R")
source("R/utils.R")

#' Sends a ["request-slice"](https://github.com/flowr-analysis/flowr/wiki/Interface#the-slice-request) request
#'
#' @param filetoken The filetoken of the file to be sliced
#' (retrieved by a previous "file-analysis" request)
#' @param criteria The slice's criteria
#' @param id The id of the request
#' @param con The connection to the server
#' @return A list containing the id and the response
#'
#' @seealso [initiate_file_analysis()]
#'
#' @export
request_slice <- function(filetoken,
                          criteria,
                          id = get_new_id(),
                          con = connect()) {
  request <- fromRJSON('{
    "type":      "request-slice",
    "id":        "%s",
    "filetoken": "%s",
    "criterion": %s
  }', filetoken, jsonlite::toJSON(criteria))
  res <- send_request(con, request)

  if (handle_err_result(res)) {
    return(NULL)
  }

  return(list(id = id, res = res))
}
