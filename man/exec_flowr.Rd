% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flowr_local.R
\name{exec_flowr}
\alias{exec_flowr}
\title{Executes a local version of the flowR CLI with the given arguments in the given directory.
This function expects Node and flowR to have been installed using \code{\link[=install_node]{install_node()}} and \code{\link[=install_flowr]{install_flowr()}} prior.}
\usage{
exec_flowr(
  args,
  verbose = FALSE,
  base_dir = get_default_node_base_dir(),
  background = FALSE
)
}
\arguments{
\item{args}{The arguments to pass to the flowR CLI, as a character vector.}

\item{verbose}{Whether to print out information about the commands being executed.}

\item{base_dir}{The base directory that Node and flowR were installed in. By default, this uses the package's installation directory through \code{\link[=get_default_node_base_dir]{get_default_node_base_dir()}}.}

\item{background}{Whether the command should be executed as a background process.}
}
\value{
The return value of the \code{\link[=exec_node_command]{exec_node_command()}} call, which is the exit code if background is false, or the pid if background is true.
}
\description{
Executes a local version of the flowR CLI with the given arguments in the given directory.
This function expects Node and flowR to have been installed using \code{\link[=install_node]{install_node()}} and \code{\link[=install_flowr]{install_flowr()}} prior.
}
