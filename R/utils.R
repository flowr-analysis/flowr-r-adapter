fromRJSON <- function(x, ...) {
  return(jsonlite::fromJSON(sprintf(x, ...),
    simplifyVector = FALSE,
    simplifyDataFrame = FALSE,
    simplifyMatrix = FALSE
  ))
}

handle_err_result <- function(res) {
  if (res$type == "error") {
    return(TRUE)
  }
  return(FALSE)
}

make_id_provider <- function(next_id = 0) {
  function()
    next_id <<- next_id + 1
}
get_new_id <- make_id_provider()

get_filetoken <- function(filepath = NULL, content = NULL) {
  if (!is.null(filepath)) {
    if (!file.exists(filepath)) {
      stop("File does not exist: ", filepath)
    }
    token <- digest::digest(readLines(filepath))
  } else if (!is.null(content)) {
    token <- digest::digest(content)
  }
  return(token)
}
