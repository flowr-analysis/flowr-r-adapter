#' Sends a ["file-analysis"](https://github.com/flowr-analysis/flowr/wiki/Interface#the-analysis-request) request
#'
#' Note that either `filepath` or `content` must be provided, but never both.
#'
#' @param con The connection to the server
#' @param filepath A single or multiple file paths (must be visible to the server)
#' @param content The R code to be analyzed
#' @param cfg Weather to include the control flow graph in the response
#' @param id The id of the request
#' @param filetoken The filetoken of the file to be analyzed
#' @return A list containing the id, filetoken and the response
#'
#' @seealso [connect()]
#'
#' @export
request_file_analysis <- function(con,
                                  filepath = rlang::missing_arg(),
                                  content = rlang::missing_arg(),
                                  cfg = FALSE,
                                  id = get_new_id(),
                                  filetoken = get_filetoken(filepath, content)) {
  stopifnot(rlang::is_missing(filepath) || rlang::is_missing(content), !rlang::is_missing(filepath) || !rlang::is_missing(content))

  if (!rlang::is_missing(filepath)) {
    stopifnot(is.character(filepath) || is.vector(filepath))
    filepath <- normalizePath(filepath, mustWork = FALSE)
    request <- list(
      type = "request-file-analysis",
      id = id,
      filetoken = filetoken,
      filepath = filepath,
      cfg = cfg
    )
  } else if (!rlang::is_missing(content)) {
    stopifnot(is.character(content))
    request <- list(
      type = "request-file-analysis",
      id = id,
      filetoken = filetoken,
      content = content,
      cfg = cfg
    )
  }
  return(c(
    send_request_handle_response(con, request),
    list(filetoken = filetoken)
  ))
}
