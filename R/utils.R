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

next_id <- 0
get_new_id <- function() {
  # next_id <<- next_id + 1
  return(next_id)
}

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
