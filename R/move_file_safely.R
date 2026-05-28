.move_file_exists <- function(path) {
  file.exists(path)
}

.move_file_rename <- function(from, to) {
  file.rename(from, to)
}

.move_file_copy <- function(from, to, overwrite = TRUE) {
  file.copy(from = from, to = to, overwrite = overwrite)
}

.move_file_unlink <- function(path) {
  unlink(path)
}

#' Move file safely across filesystems
#'
#' @description Internal helper that first attempts `file.rename()` and falls
#'   back to copy + delete when source and destination are on different devices
#'   (common in Docker bind mounts).
#'
#' @param from [character] Source file path
#' @param to [character] Destination file path
#'
#' @return `TRUE` on success, `FALSE` otherwise
#' @keywords internal
move_file_safely <- function(from, to) {
  if (!.move_file_exists(from)) {
    return(FALSE)
  }

  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)

  renamed <- .move_file_rename(from, to)
  if (isTRUE(renamed)) {
    return(TRUE)
  }

  copied <- .move_file_copy(from = from, to = to, overwrite = TRUE)
  if (!isTRUE(copied) || !.move_file_exists(to)) {
    return(FALSE)
  }

  .move_file_unlink(from)
  TRUE
}
