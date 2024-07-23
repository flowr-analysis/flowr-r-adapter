source("R/connect.R")
source("R/utils.R")

#' Sends a "file-analysis" request
#'
#' @param filepath The path to the file (must be visible to the server)
#' @param id The id of the request
#' @param filetoken The filetoken of the file to be analyzed
#' @param con The connection to the server
#' @return A list containing the id, filetoken and the response
#'
#' @export
# TODO: support content and filepath
request_file_analysis <- function(filepath,
                                  id = get_new_id(),
                                  filetoken = get_filetoken(),
                                  con = connect()) {
  filepath <- normalizePath(filepath, mustWork = FALSE)
  filetoken <- get_filetoken(filepath)

  request <- fromRJSON('{
    "type":      "request-file-analysis",
    "id":        "%s",
    "filetoken": "%s",
    "filepath":  "%s"
  }', id, filetoken, filepath)
  res <- send_request(con, request)

  if (handle_err_result(res)) {
    return(NULL)
  }

  return(list(id = id, filetoken = filetoken, res = res))
}
