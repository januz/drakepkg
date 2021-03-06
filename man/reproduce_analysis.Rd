% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reproduce_analysis.R
\name{reproduce_analysis}
\alias{reproduce_analysis}
\title{Reproduce the analysis}
\usage{
reproduce_analysis(to = getwd(), overwrite = FALSE, re_run = FALSE)
}
\arguments{
\item{to}{Character scalar, file path, where to copy analysis structure
(default = current working directory).}

\item{overwrite}{Logical, whether to overwrite the analysis directory structure (default = \code{FALSE})}

\item{re_run}{Logical, whether to re-run the analysis from scratch.}
}
\description{
Copy bundled analysis directory structure to selected directory and
reproduce analysis.
}
\note{
This function will copy the analysis directory structure supplied with the package
and the \code{.drake/} directory into the user's working directory (or the directory supplied
to the \code{to} parameter) and reproduce the analysis by running \code{drake::make()}.
}
