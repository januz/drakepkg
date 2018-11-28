#' @title get file list
#' @description get list of all files and directories in the selected directory that
#'   are part of the analysis directory structure.
#' @param dir Character, file path, where the files are searched for.
get_file_list <- function(dir){
  file_list <- list.files(
    pkgenv$source_path,
    full.names = TRUE,
    recursive = TRUE
  )

  if (length(file_list) == 0) {
    stop("This package doesn't seem to include an analysis directory structure.")
  }

  if (dir != pkgenv$source_path) {
    source <- list.files(
      pkgenv$source_path,
      full.names = TRUE
    )

    find_files <- function(path) length(list.files(path)) == 0
    is_file <- unlist(lapply(source, find_files))

    base_names <- gsub(
      paste(pkgenv$source_path, "/", sep = ""),
      paste(dir, "/", sep = ""),
      source
    )

    dirs <- base_names[!is_file]
    files <- base_names[is_file]

    file_list <- c(
      list.files(
        dirs,
        full.names = TRUE,
        recursive = TRUE
      ),
      files
    )
  }

  file_list
}

#' @title compute hashes
#' @description compute md5 hashes for all files in specified directory.
#' @param dir Character, file path.
compute_hashes <- function(dir){
  file_paths <- get_file_list(dir)
  file_names <- gsub(paste(dir, "/", sep = ""), "", file_paths)
  hashes <- tools::md5sum(file_paths)

  tibble::tibble(
    file_name = file_names,
    hash = hashes
  )
}


#' @title compare directories
#' @description compare analysis directories in source and target directory
#' @param to Character scalar, file path, where to copy analysis structure
#'  (default = current working directory).
compare_dirs <- function(to = getwd()){
  hashes_source <- compute_hashes(pkgenv$source_path)
  hashes_target <- compute_hashes(to)

  hashes <- dplyr::full_join(
    hashes_source,
    hashes_target,
    by = "file_name"
  )

  files_mis <- is.na(hashes$hash.y)
  files_new <- is.na(hashes$hash.x)
  files_mod <- (hashes$hash.x != hashes$hash.y) %>%
    replace(., is.na(.), FALSE)

  if (sum(files_mis) > 0) {
    status <- "missing"
    if (sum(files_mis) > 0 & sum(files_mis) < length(files_mis)) {
      message("Missing file(s) in target dir:")
      cat(hashes$file_name[files_mis], sep = "\n")
    }
  }

  if (sum(files_new) > 0 | sum(files_mod) > 0) {
    status <- "modified"
    if (sum(files_new) > 0) {
      message("New file(s) in target dir:")
      cat(hashes$file_name[files_new], sep = "\n")
    }
    if (sum(files_mod, na.rm = TRUE) > 0) {
      message("Modified file(s) in target dir:")
      cat(hashes$file_name[files_mod], sep = "\n")
    }
  }

  if (sum(files_mis) == 0 & sum(files_new) == 0 & sum(files_mod) == 0) {
    status <- "identical"
  }

  status
}

#' @title Copy analysis directory structure
#' @description Copy the analysis directory structure of a packaged `drake` workflow to the
#'   selected directory.
#' @param to Character scalar, file path, where to copy the workflow's files to
#'   (default = current working directory).
#' @param drake Logical, whether to copy `.drake` directory (default = `TRUE`).
#' @param overwrite Logical, whether to overwrite the analysis directory structure (default = `FALSE`)
#' @note This function will copy the analysis directory structure supplied with the package
#'   and the `.drake/` directory into the user's working directory (or the directory supplied
#'   to the `to` parameter).
#'
#' @export
copy_analysis <- function(to = getwd(), drake = TRUE, overwrite = FALSE) {
  status <- compare_dirs(to)

  dir_names <- list.files(
    pkgenv$source_path,
    all.files = TRUE,
    no.. = TRUE
  )
  dir_names_target <- paste(to, dir_names, sep = "/")
  no_symlink <- !nzchar(Sys.readlink(dir_names_target))

  is_local_pkg <- grepl("/inst$", system.file(package = pkgenv$pkg_name))

  if (status == "modified" & !overwrite) {
    stop(
      "Analysis directory structure has been changed locally! ",
      "To overwrite, run again with `overwrite = TRUE`"
    )
  } else if (is_local_pkg & sum(no_symlink) > 0 & !overwrite) {
    message("Following files in the analysis directory structure are not symlinked:")
    cat(dir_names_target[no_symlink], sep = "\n")
    stop("To use symlinks, run again with `overwrite = TRUE`")
  } else if (status == "identical" & !overwrite){
    drake_source <- paste("./inst", pkgenv$source_dir, ".drake", sep = "/")
    drake_target <- paste(to, ".drake", sep = "/")
    drake_exists <- file.exists(drake_target)

    if (!drake) {
      unlink(drake_target, recursive = TRUE)
      message("Analysis directory structure is already in place.")
    } else if (drake & !drake_exists) {
      if (is_local_pkg) {
        file.symlink(drake_source, to)
      } else {
        file.copy(drake_source, to)
      }
      message("Copying analysis directory structure...")
    } else {
      message("Analysis directory structure is already in place.")
    }
  } else {
    message("Copying analysis directory structure...")

    purrr::walk(
      dir_names_target,
      ~ unlink(.x, recursive = TRUE)
    )

    if (!drake) {
      dir_names <- dir_names[! dir_names %in% ".drake"]
    }

    if (is_local_pkg) {
      purrr::walk2(
        paste("./inst", pkgenv$source_dir, dir_names, sep = "/"),
        to,
        ~ file.symlink(.x, .y)
      )
    } else {
      purrr::walk(
        paste(pkgenv$source_path, dir_names, sep = "/"),
        ~ file.copy(.x, to = to, recursive = TRUE)
      )
    }
  }
}

#' @title Delete analysis directory structure
#' @description Delete the analysis directory structure of a packaged `drake` workflow from a
#'   selected directory.
#' @param to Character scalar, file path, where to delete the directories from
#'   (default = current working directory).
#' @note This function will delete a previously copied analysis directory structure from the user's
#'   working directory (or the directory supplied to the `to` parameter).
#'
#' @export
delete_analysis <- function(to = getwd()) {
  dir_names <- list.files(
    pkgenv$source_path,
    all.files = TRUE,
    no.. = TRUE
  )
  dir_names_target <- paste(to, dir_names, sep = "/")

  message("Deleting analysis directory structure...")

  purrr::walk(
    dir_names_target,
    ~ unlink(.x, recursive = TRUE)
  )
}

#' @title Reproduce the analysis
#' @description Copy bundled analysis directory structure to selected directory and
#'   reproduce analysis.
#' @param to Character scalar, file path, where to copy analysis structure
#'  (default = current working directory).
#' @param overwrite Logical, whether to overwrite the analysis directory structure (default = `FALSE`)
#' @param re_run Logical, whether to re-run the analysis from scratch.
#' @note This function will copy the analysis directory structure supplied with the package
#'   and the `.drake/` directory into the user's working directory (or the directory supplied
#'   to the `to` parameter) and reproduce the analysis by running `drake::make()`.
#'
#' @export
reproduce_analysis <- function(to = getwd(), overwrite = FALSE, re_run = FALSE) {
  message("Reproducing analysis included in package '", getPackageName()[[1]], "':\n")
  copy_analysis(to = to, overwrite = overwrite)

  if (to != getwd()) {
    message("Switching working directory to '", to, "'")
    setwd(to)
  }

  if (re_run){
    unlink(paste(to, ".drake", sep = "/"), recursive = TRUE)
    message("Re-running analysis from scratch...")
  } else {
    message("Checking consistency of provided analysis...")
  }

  plan <- get_plan()
  drake::make(plan)
}
