# Graphs and overview ---------------------------------------------------------

# TRUE if `items` is a non-empty list of dependency records (each with a nodeId).
.flowr_is_dep_segment <- function(items) {
  is.list(items) && length(items) > 0 &&
    all(vapply(items, function(it) is.list(it) && !is.null(it$nodeId), logical(1)))
}

#' Turn a flowR graph into vertex and edge data frames
#'
#' **Experimental.** Runs the `dataflow` (or `call-graph`) query and returns the
#' graph as tidy `vertices` (`id`, `tag`) and `edges` (`from`, `to`, `type`) data
#' frames, ready for [flowr_as_igraph()] or any graph tool.
#'
#' @inheritParams slice
#' @param type `"dataflow"` (default) or `"call-graph"`.
#' @return A `flowr_graph`: a list of `vertices` and `edges` data frames.
#' @seealso [flowr_as_igraph()]
#' @inheritSection slice Analysing the current project
#' @export
#' @examples
#' \dontrun{
#' g <- flowr_graph("x <- 1\ny <- x + 1")
#' head(g$vertices)
#' head(g$edges)
#' }
flowr_graph <- function(code = NULL, file = NULL, folder = NULL,
                        type = c("dataflow", "call-graph"), session = NULL) {
  type <- match.arg(type)
  res <- query(code = code, file = file, folder = folder, query = type, session = session)[[type]]
  g <- res$graph %||% res
  verts <- g$vertexInformation %||% list()
  vertices <- data.frame(
    id = vapply(verts, function(e) as.character(e[[1]]), character(1)),
    tag = vapply(verts, function(e) e[[2]]$tag %||% NA_character_, character(1)),
    stringsAsFactors = FALSE
  )
  from <- character(0); to <- character(0); etype <- character(0)
  for (e in g$edgeInformation %||% list()) {
    src <- as.character(e[[1]])
    for (te in e[[2]]) {
      ty <- te[[2]]$types %||% te[[2]]$type
      from <- c(from, src)
      to <- c(to, as.character(te[[1]]))
      etype <- c(etype, if (is.null(ty)) NA_character_ else paste(unlist(ty), collapse = ","))
    }
  }
  edges <- data.frame(from = from, to = to, type = etype, stringsAsFactors = FALSE)
  # ensure every edge endpoint is a vertex (built-in functions are pseudo-nodes)
  missing <- setdiff(unique(c(edges$from, edges$to)), vertices$id)
  if (length(missing) > 0) {
    vertices <- rbind(vertices,
                      data.frame(id = missing, tag = "built-in", stringsAsFactors = FALSE))
  }
  structure(list(vertices = vertices, edges = edges, kind = type), class = "flowr_graph")
}

#' @export
print.flowr_graph <- function(x, ...) {
  cat(sprintf("<flowr_graph: %s | %d vertices, %d edges>\n",
              x$kind, nrow(x$vertices), nrow(x$edges)))
  invisible(x)
}

#' Convert a flowR graph to an igraph object
#'
#' **Experimental.**
#'
#' @param x A [flowr_graph], or `code`/`file` passed on to [flowr_graph()].
#' @param ... Passed to [flowr_graph()] when `x` is not already a graph.
#' @return An `igraph` object (requires the suggested `igraph` package).
#' @export
#' @examples
#' \dontrun{
#' ig <- flowr_as_igraph(flowr_graph("x <- 1\ny <- x + 1"))
#' igraph::vcount(ig)
#' }
flowr_as_igraph <- function(x, ...) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    .flowr_stop("flowr_as_igraph() needs the 'igraph' package: install.packages(\"igraph\")")
  }
  if (!inherits(x, "flowr_graph")) {
    x <- flowr_graph(x, ...)
  }
  igraph::graph_from_data_frame(x$edges, directed = TRUE, vertices = x$vertices)
}

# The package's GitHub repo URL from the local package db (URL / BugReports),
# or NULL when the package is not installed or has no GitHub link.
.flowr_pkg_url <- function(pkg) {
  d <- tryCatch(utils::packageDescription(pkg), error = function(e) NA)
  if (length(d) == 1L && is.na(d)) {
    return(NULL)
  }
  fields <- paste(c(d$URL, d$BugReports), collapse = " ")
  m <- regmatches(fields, regexpr("https?://github\\.com/[A-Za-z0-9._-]+/[A-Za-z0-9._-]+",
                                  fields))
  if (length(m) == 0) {
    return(NULL)
  }
  sub("\\.git$", "", m[[1]])
}

# Map a node id to the variable it is assigned to (e.g. `p <- plot()` -> plot's
# call id -> "p"), so named plots can be shown by name.
.flowr_assignment_names <- function(ast) {
  m <- list()
  visit_node(ast, function(n) {
    if (identical(n$type, "RBinaryOp") && !is.null(n$operator)) {
      if (n$operator %in% c("<-", "=", "<<-") &&
          !is.null(n$rhs$info$id) && !is.null(n$lhs$content)) {
        m[[as.character(n$rhs$info$id)]] <<- n$lhs$content
      } else if (n$operator %in% c("->", "->>") &&
                 !is.null(n$lhs$info$id) && !is.null(n$rhs$content)) {
        m[[as.character(n$lhs$info$id)]] <<- n$rhs$content
      }
    }
    TRUE
  })
  m
}

#' An overview of a script's dependencies, ready to slice
#'
#' **Experimental.** Runs the `dependencies` query and returns the libraries
#' loaded, files sourced, data read/written, visualizations produced and tests
#' run. Every item gains a `criterion` field (a flowR node-id criterion) that
#' plugs straight into [slice()], so you can slice on any of them, e.g. slice
#' backward on all visualizations to see the code each plot depends on.
#'
#' @inheritParams slice
#' @return A `flowr_overview`: the dependency segments (`library`, `source`,
#'   `read`, `write`, `visualize`, `test`), each item with a `criterion`.
#' @seealso [slice()], [query()]
#' @inheritSection slice Analysing the current project
#' @export
#' @examples
#' \dontrun{
#' ov <- flowr_overview("plot(1:10)\nhist(rnorm(20))")
#' # slice backward on every visualization at once:
#' crit <- vapply(ov$visualize, function(v) v$criterion, character(1))
#' slice("plot(1:10)\nhist(rnorm(20))", crit)
#' flowr_overview()   # the R package in the working directory
#' }
flowr_overview <- function(code = NULL, file = NULL, folder = NULL, session = NULL) {
  done <- .flowr_timer("overview"); on.exit(done(), add = TRUE)
  session <- .flowr_resolve_session(session)
  an <- .flowr_input_analysis(code, file, folder, cfg = FALSE, session = session)$an
  .flowr_timing_detail(.flowr_analysis_phases(an$analysis))
  # run the dependencies query against the already-analysed input (no re-analysis)
  req <- list(type = "request-query", id = .flowr_session_id(session),
              filetoken = an$filetoken, query = I(list(list(type = "dependencies"))))
  deps <- .flowr_request(session$con, req)$results$dependencies
  # reuse the same analysis for source lines and assigned names
  loc <- list()
  names_map <- list()
  tryCatch({
    ast <- an$analysis$results$normalize$ast
    loc <- make_id_to_location_map(ast)
    names_map <- .flowr_assignment_names(ast)
  }, error = function(e) NULL)
  # add a slicing criterion, line and (if assigned) name to every record, for
  # whatever segments flowR reports
  for (seg in names(deps)) {
    if (.flowr_is_dep_segment(deps[[seg]])) {
      is_lib <- identical(seg, "library")
      deps[[seg]] <- lapply(deps[[seg]], function(it) {
        it$criterion <- paste0("$", it$nodeId)
        l <- loc[[paste0(it$nodeId)]]
        it$line <- if (is.null(l)) NA_integer_ else as.integer(l[[1]])
        nm <- names_map[[paste0(it$nodeId)]]
        if (!is.null(nm)) it$name <- nm
        if (is_lib) {
          # installed version + GitHub URL from the local package db, if any
          it$version <- tryCatch(as.character(utils::packageVersion(it$value)),
                                 error = function(e) NA_character_)
          it$url <- .flowr_pkg_url(it$value)
        }
        it
      })
    }
  }
  structure(deps, class = "flowr_overview")
}

#' @export
print.flowr_overview <- function(x, color = .flowr_use_color(), ...) {
  segs <- Filter(function(s) .flowr_is_dep_segment(x[[s]]), names(x))
  total <- sum(vapply(segs, function(s) length(x[[s]]), integer(1)))
  cat(.flowr_ansi(sprintf("overview | %d item%s in %d categor%s", total,
                          if (total == 1) "" else "s", length(segs),
                          if (length(segs) == 1) "y" else "ies"), "1", color),
      "\n", sep = "")
  if (total == 0) {
    cat("  (no dependencies found)\n")
    return(invisible(x))
  }
  item_line <- function(it, indent) {
    name <- paste0(strrep("  ", indent), it$name %||% it$value %||% it$functionName %||% "")
    ver <- if (!is.null(it$version) && !is.na(it$version)) it$version else ""
    tag <- if (nzchar(ver)) paste0("[pkg db: ", ver, "]") else ""
    visible <- nchar(name) + if (nzchar(tag)) nchar(tag) + 1L else 0L
    # pkg db tag: slightly darker gray. Underline + hyperlink it only when the
    # terminal can actually follow the link, so we never advertise a dead link.
    tag_out <- if (nzchar(tag)) {
      linkable <- !is.null(it$url) && color && .flowr_hyperlinks_supported()
      styled <- .flowr_ansi(tag, if (linkable) "4;90" else "90", color)
      .flowr_hyperlink(styled, it$url, color)
    } else {
      ""
    }
    label <- if (nzchar(tag)) paste0(name, " ", tag_out) else name
    ln <- if (is.null(it$line) || is.na(it$line)) "" else sprintf("line %-4s", it$line)
    # link the function to its definition only if flowR gave us the location
    via <- .flowr_ansi(sprintf("via %-12s", it$functionName %||% ""), "2", color)
    if (!is.null(it$definitionUrl) && nzchar(it$definitionUrl)) {
      via <- .flowr_hyperlink(via, it$definitionUrl, color)
    }
    cat("  ", label, strrep(" ", max(1L, 30L - visible)), " ",
        .flowr_ansi(formatC(ln, width = -9), "2", color), via, " ",
        .flowr_ansi(it$criterion %||% "", "36", color), "\n", sep = "")
  }
  for (s in segs) {
    cat(.flowr_ansi(paste0("-- ", s, " ", strrep("-", max(0, 44 - nchar(s)))), "2", color),
        "\n", sep = "")
    items <- x[[s]]
    ids <- vapply(items, function(it) as.character(it$nodeId %||% ""), character(1))
    # nest items that modify another (e.g. lines() under its plot) via linkedIds
    parent <- rep(NA_integer_, length(items))
    for (i in seq_along(items)) {
      lk <- items[[i]]$linkedIds
      if (!is.null(lk) && length(lk) > 0) {
        j <- setdiff(which(ids %in% as.character(unlist(lk))), i)
        if (length(j) > 0) parent[i] <- j[1]
      }
    }
    printed <- logical(length(items))
    for (i in which(is.na(parent))) {
      item_line(items[[i]], 0)
      printed[i] <- TRUE
      for (k in which(parent == i)) {
        item_line(items[[k]], 1)
        printed[k] <- TRUE
      }
    }
    for (i in which(!printed)) {
      item_line(items[[i]], 0)     # orphans / cycles
    }
  }
  invisible(x)
}
