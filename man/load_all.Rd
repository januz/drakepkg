% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{load_all}
\alias{load_all}
\title{Load complete package and expose import to drake}
\usage{
load_all(document = FALSE)
}
\arguments{
\item{document}{run roxygen (default FALSE)}
}
\description{
Convenience wrapper that executes \code{devtools::load_all()} and
\code{drake::expose_imports()} to reload a package containing a drake workflow
}
