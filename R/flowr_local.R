#' Installs the given version of Node.js on the local system in the given directory.
#'
#' @param node_ver The versino of Node to install
#' @param verbose Whether to print out information about the commands being executed.
#' @param base_dir The base directory to install Node in. By default, this uses the package's installation directory through [get_default_node_base_dir()].
#'
#' @export
install_node <- function(node_ver, verbose = FALSE, base_dir = get_default_node_base_dir()) {
  os <- get_os()
  arch <- switch(Sys.info()[["machine"]],
    "x86-64" = "x64",
    "x86_64" = "x64",
    "x86-32" = "x86",
    "x86_32" = "x86",
    "arm64" = "arm64",
    stop(paste0("Unsupported architecture ", Sys.info()[["machine"]]))
  )

  if (verbose) {
    print(paste0("Installing node ", node_ver, " in ", base_dir))
  }

  if (dir.exists(base_dir)) {
    unlink(base_dir, recursive = TRUE)
    if (verbose) {
      print("Removing old node installation")
    }
  }
  dir.create(base_dir)

  # url example: https://nodejs.org/dist/v22.5.1/node-v22.5.1-win-x86.zip
  file_type <- if (os == "win") "zip" else "tar.gz"
  node_archive_dest <- file.path(base_dir, paste0("node.", file_type))
  node_file_name <- sprintf("node-v%s-%s-%s", node_ver, os, arch)
  utils::download.file(sprintf("https://nodejs.org/dist/v%s/%s.%s", node_ver, node_file_name, file_type), node_archive_dest)
  if (verbose) {
    print(paste0("Downloaded node archive to ", node_archive_dest))
  }

  if (file_type == "zip") {
    utils::unzip(node_archive_dest, exdir = base_dir)
  } else {
    utils::untar(node_archive_dest, exdir = base_dir)
  }
  unlink(node_archive_dest)

  if (verbose) {
    print(paste0("Extracted node archive to ", file.path(base_dir, node_file_name)))
  }
}

#' Installs the given version of flowR on the local system in the given directory.
#' This function expects Node to have been installed using [install_node()] prior.
#'
#' @param flowr_ver The version of flowR to install.
#' @param verbose Whether to print out information about the commands being executed.
#' @param base_dir The base directory that Node was installed in, and where flowR should be installed. By default, this uses the package's installation directory through [get_default_node_base_dir()].
#' @param std_out The std_out parameter passed to the sys call. See [sys::exec_background()] and [sys::exec_wait()] for more info.
#' @param std_err The std_err parameter passed to the sys call. See [sys::exec_background()] and [sys::exec_wait()] for more info.
#' @return The return value of the [exec_node_command()] call.
#'
#' @export
install_flowr <- function(flowr_ver, verbose = FALSE, base_dir = get_default_node_base_dir(), std_out = TRUE, std_err = TRUE) {
  exec_node_command("npm", c("install", "-g", paste0("--prefix=", get_node_exe_dir(base_dir)), paste0("@eagleoutice/flowr@", flowr_ver)), verbose, base_dir, FALSE, std_out, std_err)
}

#' Executes a local version of the flowR CLI with the given arguments in the given directory.
#' This function expects Node and flowR to have been installed using [install_node()] and [install_flowr()] prior.
#'
#' @param args The arguments to pass to the flowR CLI, as a character vector.
#' @param verbose Whether to print out information about the commands being executed.
#' @param base_dir The base directory that Node and flowR were installed in. By default, this uses the package's installation directory through [get_default_node_base_dir()].
#' @param background Whether the command should be executed as a background process.
#' @param std_out The std_out parameter passed to the sys call. See [sys::exec_background()] and [sys::exec_wait()] for more info.
#' @param std_err The std_err parameter passed to the sys call. See [sys::exec_background()] and [sys::exec_wait()] for more info.
#' @return The return value of the [exec_node_command()] call, which is the exit code if background is false, or the pid if background is true.
#'
#' @export
exec_flowr <- function(args, verbose = FALSE, base_dir = get_default_node_base_dir(), background = FALSE, std_out = TRUE, std_err = TRUE) {
  # we installed flowr globally (see above) in the scope of our local node installation, so we can find it here
  node_modules <- if (get_os() == "win") "node_modules" else file.path("lib", "node_modules")
  flowr_path <- file.path(get_node_exe_dir(base_dir), node_modules, "@eagleoutice", "flowr", "cli", "flowr.js")
  exec_node_command("node", c(flowr_path, args), verbose, base_dir, background, std_out, std_err)
}

#' Executes a local version of the flowR CLI with the given arguments in the given directory.
#' This function expects docker to exist on the system.
#'
#' @param docker_args Additional arguments to pass to docker, as a character vector.
#' @param flowr_ver The version of flowR to use
#' @param flowr_args The arguments to pass to the flowR CLI, as a character vector.
#' @param verbose Whether to print out information about the commands being executed.
#' @param cmd The command to use for docker. Defaults to "docker".
#' @param background Whether the command should be executed as a background process.
#' @param std_out The std_out parameter passed to the sys call. See [sys::exec_background()] and [sys::exec_wait()] for more info.
#' @param std_err The std_err parameter passed to the sys call. See [sys::exec_background()] and [sys::exec_wait()] for more info.
#' @return The return value of the [exec_node_command()] call, which is the exit code if background is false, or the pid if background is true.
#'
#' @export
exec_flowr_docker <- function(docker_args, flowr_ver, flowr_args, verbose = FALSE, cmd = "docker", background = FALSE, std_out = TRUE, std_err = TRUE) {
  exec_docker_command(c("run", "--rm", docker_args, paste0("eagleoutice/flowr:", flowr_ver), flowr_args), verbose, cmd, background, std_out, std_err)
}

#' Executes the given Node subcommand in the given arguments in the given directory.
#' This function expects Node to have been installed using [install_node()].
#'
#' @param app The node subcommand to run, which can be one of "node", "npm", or "npx".
#' @param args The arguments to pass to the Node command, as a character vector.
#' @param verbose Whether to print out information about the commands being executed.
#' @param base_dir The base directory that Node was installed in. By default, this uses the package's installation directory through [get_default_node_base_dir()].
#' @param background Whether the command should be executed as a background process.
#' @param std_out The std_out parameter passed to the sys call. See [sys::exec_background()] and [sys::exec_wait()] for more info.
#' @param std_err The std_err parameter passed to the sys call. See [sys::exec_background()] and [sys::exec_wait()] for more info.
#' @return The return value of the call, which is the exit code if background is false, or the pid if background is true.
#'
#' @export
exec_node_command <- function(app = c("node", "npm", "npx"), args, verbose = FALSE, base_dir = get_default_node_base_dir(), background = FALSE, std_out = TRUE, std_err = TRUE) {
  # linux/mac have binaries in the bin subdirectory, windows has node.exe and npm/npx.cmd in the root, bleh
  path <- if (get_os() == "win") paste0(app, if (app == "node") ".exe" else ".cmd") else file.path("bin", app)
  cmd <- file.path(get_node_exe_dir(base_dir), path)
  if (verbose) {
    print(paste0("Executing ", cmd, " ", paste0(args, collapse = " ")))
  }
  if (background) {
    return(sys::exec_background(cmd, args, std_out, std_err))
  } else {
    return(sys::exec_wait(cmd, args, std_out, std_err))
  }
}

#' Executes the given docker command in the given directory.
#' This function expects docker to exist on the system.
#'
#' @param args The arguments to pass to the docker command, as a character vector.
#' @param verbose Whether to print out information about the commands being executed.
#' @param cmd The command to use for docker. Defaults to "docker".
#' @param background Whether the command should be executed as a background process.
#' @param std_out The std_out parameter passed to the sys call. See [sys::exec_background()] and [sys::exec_wait()] for more info.
#' @param std_err The std_err parameter passed to the sys call. See [sys::exec_background()] and [sys::exec_wait()] for more info.
#' @return The return value of the call, which is the exit code if background is false, or the pid if background is true.
#'
#' @export
exec_docker_command <- function(args, verbose = FALSE, cmd = "docker", background = FALSE, std_out = TRUE, std_err = TRUE) {
  if (verbose) {
    print(paste0("Executing ", cmd, " ", paste0(args, collapse = " ")))
  }
  if (background) {
    return(sys::exec_background(cmd, args, std_out, std_err))
  } else {
    return(sys::exec_wait(cmd, args, std_out, std_err))
  }
}

#' Returns the default node base directory to use when installing Node, which is the directory that the package with the given name is installed in.
#'
#' @param pkg_dir_name The name of the package to find the installation directory of. By default, this is "flowr", the name of this package.
#' @return The default base directory to use when installing Node.
#'
#' @export
get_default_node_base_dir <- function(pkg_dir_name = "flowr") {
  # we find the directory to install node into by finding the directory that
  # the currently running instance of the package is (likely) installed in.
  # this may seem like a terrible solution but it's the best one i could come up with :(
  for (path in .libPaths()) {
    for (dir in list.dirs(path, full.names = FALSE, recursive = FALSE)) {
      if (dir == pkg_dir_name) {
        return(file.path(path, dir, "_node"))
      }
    }
  }
  stop(paste0("Could not find ", pkg_dir_name, " directory in any libPaths"))
}

get_node_exe_dir <- function(base_dir) {
  if (dir.exists(base_dir)) {
    # we installed node like _node/node-versionblahblah/node.exe etc, and since we
    # delete the old installation every time, we expect a single directory of this form
    node_dirs <- list.dirs(base_dir, recursive = FALSE)
    if (length(node_dirs) == 1) {
      return(node_dirs[[1]])
    }
  }
  stop(paste0("Node not installed correctly in ", base_dir))
}

get_os <- function() {
  return(switch(Sys.info()[["sysname"]],
    Windows = "win",
    Linux = "linux",
    Darwin = "darwin",
    stop(paste0("Unsupported operating system ", Sys.info()[["sysname"]]))
  ))
}
