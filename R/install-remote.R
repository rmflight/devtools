#' Install a remote package.
#'
#' This:
#' \enumerate{
#'   \item downloads source bundle
#'   \item decompresses & checks that it's a package
#'   \item adds metadata to DESCRIPTION
#'   \item calls install
#' }
#' @noRd
install_remote <- function(remote, ..., quiet = FALSE) {
  stopifnot(is.remote(remote))

  bundle <- remote_download(remote, quiet = quiet)
  on.exit(unlink(bundle), add = TRUE)

  source <- source_pkg(bundle, subdir = remote$subdir)
  on.exit(unlink(source, recursive = TRUE), add = TRUE)

  # Because we've modified DESCRIPTION, its original MD5 value is wrong
  clear_description_md5(source)

  install(source,
    metadata = remote_metadata(remote, bundle, source),
      ..., quiet = quiet)
}

install_remotes <- function(remotes, ...) {
  invisible(vapply(remotes, install_remote, ..., FUN.VALUE = logical(1)))
}

# Add metadata
add_metadata <- function(pkg_path, meta) {
  path <- file.path(pkg_path, "Meta", "package.rds")
  if (file.exists(path)) {
    pkg_desc <- readRDS(path)
    desc <- as.list(pkg_desc$DESCRIPTION)
    desc <- modifyList(desc, meta)
    pkg_desc$DESCRIPTION <- setNames(as.character(desc), names(desc))
    saveRDS(pkg_desc, path)
  }

  path <- file.path(pkg_path, "DESCRIPTION")
  if (file.exists(path)) {
    desc <- read_dcf(path)
    desc <- modifyList(desc, meta)
    write_dcf(path, desc)
  }
}

# Modify the MD5 file - remove the line for DESCRIPTION
clear_description_md5 <- function(pkg_path) {
  path <- file.path(pkg_path, "MD5")

  if (file.exists(path)) {
    text <- readLines(path)
    text <- text[!grepl(".*\\*DESCRIPTION$", text)]

    writeLines(text, path)
  }
}

remote <- function(type, ...) {
  structure(list(...), class = c(paste0(type, "_remote"), "remote"))
}
is.remote <- function(x) inherits(x, "remote")

remote_download <- function(x, quiet = FALSE) UseMethod("remote_download")
remote_metadata <- function(x, bundle = NULL, source = NULL) UseMethod("remote_metadata")

#' @export
remote_metadata.package <- function(x, bundle = NULL, source = NULL) {
  list(
    RemoteType = "local",
    RemoteUrl = x$path,
    RemoteSha = if (git_committed(x$path)) git_sha1(path = x$path)
  )
}
