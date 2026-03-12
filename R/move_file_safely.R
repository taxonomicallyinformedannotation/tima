#' Move file safely across filesystems
#'
#' @description Internal helper that first attempts `file.rename()` and falls
#'   back to copy + delete when source and destination are on different devices
#'   (common in Docker bind mounts).
#'
#' @param from Source file path
#' @param to Destination file path
#'
#' @return `TRUE` on success, `FALSE` otherwise
#' @keywords internal
move_file_safely <- function(from, to) {
  if (!file.exists(from)) {
    return(FALSE)
  }

  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)

  renamed <- suppressWarnings(file.rename(from, to))
  if (isTRUE(renamed)) {
    return(TRUE)
  }

  copied <- file.copy(from = from, to = to, overwrite = TRUE)
  if (!isTRUE(copied) || !file.exists(to)) {
    return(FALSE)
  }

  unlink(from)
  TRUE
}

