#' Visits the set of nodes and all of their children, invoking the callback for each visited node
#'
#' @param nodes The list or array of nodes to visit
#' @param callback The callback function to invoke for each node. The callback should return FALSE to stop visiting the children of the node, or anything else to continue.
#'
#' @return FALSE if the visitor was stopped by the callback.
#'
#' @noRd
visit_nodes <- function(nodes, callback) {
  if (!is.null(nodes)) {
    for (node in nodes) {
      res <- visit_node(node, callback)
      if (isFALSE(res)) {
        return(FALSE)
      }
    }
  }
}

#' Visits the given node and all of their children, invoking the callback for each visited node
#'
#' @param node The node to visit
#' @param callback The callback function to invoke for each node. The callback should return FALSE to stop visiting the children of the node, or anything else to continue.
#'
#' @return FALSE if the visitor was stopped by the callback.
#'
#' @noRd
visit_node <- function(node, callback) {
  if (is.null(node) || identical(node, "<>")) {
    return()
  }

  res <- callback(node)
  # Exit early if the callback returns FALSE
  if (isFALSE(res)) {
    return(FALSE)
  }

  # same logic as the builtin visitor (while explicitly specifying if an entry is a single node or a list)
  # https://github.com/Code-Inspect/flowr/blob/main/src/r-bridge/lang-4.x/ast/model/processing/visitor.ts#L22
  switch(node$type,
    RProject = {
      # flowR 2.11+ wraps each analysed file as files[[i]]$root
      for (f in node$files) {
        visit_node(f$root, callback)
      }
    },
    RFunctionCall = {
      # an unnamed call (`spec$ensure(x)`, `f()()`) carries `calledFunction` and
      # omits `named` altogether rather than sending it as false, so anything
      # other than an explicit TRUE means "unnamed"
      if (isTRUE(node$named)) {
        visit_node(node$functionName, callback)
      } else {
        visit_node(node$calledFunction, callback)
      }
      visit_nodes(node$arguments, callback)
    },
    RFunctionDefinition = {
      visit_nodes(node$parameters, callback)
      visit_node(node$body, callback)
    },
    RExpressionList = {
      visit_nodes(node$grouping, callback)
      visit_nodes(node$children, callback)
    },
    RForLoop = {
      visit_node(node$variable, callback)
      visit_node(node$vector, callback)
      visit_node(node$body, callback)
    },
    RWhileLoop = {
      visit_node(node$condition, callback)
      visit_node(node$body, callback)
    },
    RRepeatLoop = {
      visit_node(node$body, callback)
    },
    RIfThenElse = {
      visit_node(node$condition, callback)
      visit_node(node$then, callback)
      visit_node(node$otherwise, callback)
    },
    RBinaryOp = {
      visit_node(node$lhs, callback)
      visit_node(node$rhs, callback)
    },
    RPipe = {
      visit_node(node$lhs, callback)
      visit_node(node$rhs, callback)
    },
    RUnaryOp = {
      visit_node(node$operand, callback)
    },
    RParameter = {
      visit_node(node$name, callback)
      visit_node(node$defaultValue, callback)
    },
    RArgument = {
      visit_node(node$name, callback)
      visit_node(node$value, callback)
    },
    RAccess = {
      visit_node(node$accessed, callback)
      if (identical(node$operator, "[") || identical(node$operator, "[[")) {
        visit_nodes(node$access, callback)
      }
    }
  )
}

#' Visits each node in the given AST and creates a map that maps node IDs to their locations in the code.
#'
#' @param ast The node or AST root to visit
#'
#' @return The ID-to-location map, where the keys are the node IDs and the values are the locations of the nodes.
#'   Indexed with `[[`, exactly like the named list it used to be.
#'
#' @noRd
make_id_to_location_map <- function(ast) {
  # An environment, not a list: growing a named list one element at a time copies
  # the whole list on every insert (quadratic), and looking a name up in it is a
  # linear scan. Both are fine for a toy script and hopeless for a project with
  # hundreds of thousands of AST nodes. An environment inserts and looks up in
  # constant time, and `map[["id"]]` still returns NULL for a missing key.
  map <- .flowr_new_map()
  collect <- function(root, file) {
    visit_node(root, function(node) {
      # a node can carry a location and still have no `info` (unnamed calls do),
      # and an environment cannot be indexed by the zero-length key that would
      # then come out of paste0() -- such a node simply has no id to map
      id <- node$info$id
      if (!is.null(node$location) && length(id) == 1L) {
        map[[paste0(id)]] <- .flowr_loc(node$location, file)
      }
      TRUE
    })
  }
  # locations are numbered per file, so which file a node belongs to is part of
  # its address; a project root knows that, a bare AST does not
  if (identical(ast$type, "RProject")) {
    for (f in ast$files) {
      collect(f$root, f$filePath)
    }
  } else {
    collect(ast, NULL)
  }
  map
}

#' Tag a `[line1, col1, line2, col2]` location with the file it is numbered in.
#'
#' Carried as an attribute so the value stays the length-4 list every caller
#' indexes with `loc[[1]]`..`loc[[4]]`.
#'
#' @noRd
.flowr_loc <- function(loc, file) {
  if (!is.null(file) && length(file) == 1L) {
    attr(loc, "file") <- as.character(file)
  }
  loc
}

#' An empty node-id keyed map (see make_id_to_location_map).
#' @noRd
.flowr_new_map <- function() new.env(parent = emptyenv())

#' The location map of an existing slice.
#'
#' `slice()` resolves the map once and keeps it, so this is normally just a
#' lookup. Slices that predate that field (or came from another code path) fall
#' back to walking their AST.
#'
#' @param x A `flowr_slice`.
#' @return An id-keyed environment.
#' @noRd
.flowr_slice_location_map <- function(x) {
  if (is.environment(x$locations)) {
    return(x$locations)
  }
  tryCatch(make_id_to_location_map(x$analysis$results$normalize$ast),
           error = function(e) .flowr_new_map())
}

#' The node-id-to-location map for an analysis.
#'
#' flowR's `location-map` query is the authority and is asked first. Walking the
#' normalized AST is the fallback, and is worse in two ways: it only reaches the
#' node types the visitor has a branch for (on a two-file project, 26 locations
#' against the query's 59), and each node's id lives under its `info`, which
#' flowR omits when it serialises a reply in chunks -- so for a project-sized
#' analysis the walk yields nothing at all. Where both have an id they agree
#' exactly, so preferring the query only ever adds locations.
#'
#' @param an An analysis entry (as returned by `flowr_analyze()`).
#' @param session The session to ask, or NULL to use the AST alone.
#' @param files Tag each location with the file it is numbered in. FALSE for an
#'   inline snippet, which flowR analyses out of a scratch file whose path means
#'   nothing to the caller.
#' @return An id-keyed environment of length-4 locations (see `.flowr_loc`).
#'   Empty if neither source had any.
#' @noRd
.flowr_location_map <- function(an, session = NULL, files = TRUE) {
  if (!is.null(session) && !is.null(an$filetoken)) {
    map <- tryCatch(.flowr_query_location_map(an, session, files),
                    error = function(e) .flowr_new_map())
    if (length(map) > 0) {
      return(map)
    }
  }
  map <- tryCatch(make_id_to_location_map(an$analysis$results$normalize$ast),
                  error = function(e) .flowr_new_map())
  if (!isTRUE(files)) {
    for (nm in ls(map)) attr(map[[nm]], "file") <- NULL
  }
  map
}

#' Ask flowR's `location-map` query for an analysis' locations.
#' @noRd
.flowr_query_location_map <- function(an, session, files = TRUE) {
  res <- .flowr_request(session$con, list(
    type = "request-query", id = .flowr_session_id(session),
    filetoken = an$filetoken, query = I(list(list(type = "location-map")))
  ))
  m <- res$results[["location-map"]]$map
  # m$files indexes paths; each id entry is list(<file index>, <location>)
  paths <- if (isTRUE(files)) m$files else NULL
  map <- .flowr_new_map()
  for (nm in names(m$ids)) {
    entry <- m$ids[[nm]]
    loc <- entry[[2]]
    if (length(loc) >= 4) {
      map[[nm]] <- .flowr_loc(loc, paths[[as.character(entry[[1]])]])
    }
  }
  map
}
