#' @title Load complete package and expose import to drake
#' @description Convenience wrapper that executes \code{devtools::load_all()} and
#'   \code{drake::expose_imports()} to reload a package containing a drake workflow
#' @param document run roxygen (default FALSE)
#' @name load_all
#' @export
load_all <- function(document = FALSE){
  if (document) {
    try(
      devtools::document()
    )
  } else {
    try(
      devtools::load_all()
    )
  }

  loaded_pkgs <- devtools::loaded_packages()[["package"]]
  if ("drake" %in% loaded_pkgs) {
    message("Exposing imports from package ", pkgenv$pkg_name, " to `drake`")

    drake::expose_imports(
      pkgenv$pkg_name,
      character_only = TRUE,
      envir = parent.frame(n = 2)
    )
  }
}
