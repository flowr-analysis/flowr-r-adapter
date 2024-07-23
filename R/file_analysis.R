source("R/connect.R")
source("R/utils.R")

#' Sends a ["file-analysis"](https://github.com/flowr-analysis/flowr/wiki/Interface#the-analysis-request) request
#' Note that either `filepath` or `content` must be provided, but never both.
#'
#' @param filepath The path to the file (must be visible to the server)
#' @param id The id of the request
#' @param filetoken The filetoken of the file to be analyzed
#' @param con The connection to the server
#' @return A list containing the id, filetoken and the response
#'
#' @export
request_file_analysis <- function(filepath = NULL,
                                  content = NULL,
                                  id = get_new_id(),
                                  filetoken = get_filetoken(),
                                  con = connect()) {
  if (is.null(filepath) && is.null(content)) {
    stop("Either filepath or content must be provided")
  }
  if (!is.null(filepath) && !is.null(content)) {
    stop("filepath and content cannot be provided at the same time")
  }

  if (!is.null(filepath)) {
    filepath <- normalizePath(filepath, mustWork = FALSE)
    filetoken <- get_filetoken(filepath)
    request <- fromRJSON('{
      "type":      "request-file-analysis",
      "id":        "%s",
      "filetoken": "%s",
      "filepath":  "%s"
    }', id, filetoken, filepath)
  } else if (!is.null(content)) {
    request <- fromRJSON('{
      "type":      "request-file-analysis",
      "id":        "%s",
      "filetoken": "%s",
      "content":   "%s"
    }', id, filetoken, content)
  }

  res <- send_request(con, request)

  if (handle_err_result(res)) {
    return(NULL)
  }

  return(list(id = id, filetoken = filetoken, res = res))
}
