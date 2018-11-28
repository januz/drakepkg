pkgenv <- new.env(parent=emptyenv())

.onLoad <- function(libname, pkgname) {
  pkgenv[["pkg_name"]] <- getPackageName()[[1]]

  pkgenv[["source_dir"]] <- "analysis_dir"

  pkgenv[["source_path"]] <- paste(
    system.file(package = pkgenv$pkg_name), pkgenv$source_dir, sep = "/"
  )

}

