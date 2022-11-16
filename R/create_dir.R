#' @title Create directory
#'
#' @noRd
#'
#' @param export TODO
#'
#' @return TODO
#'
#' @export
#'
#' @examples TODO
create_dir <- function(export) {
  if (grepl(
    pattern = ".",
    x = export,
    fixed = TRUE
  )) {
    ifelse(
      test = !dir.exists(paths = dirname(path = export)),
      yes = dir.create(path = dirname(path = export), recursive = TRUE),
      no = paste(
        dirname(path = export),
        "exists"
      )
    )
  } else {
    ifelse(
      test = !dir.exists(paths = export),
      yes = dir.create(path = export, recursive = TRUE),
      no = paste(
        export,
        "exists"
      )
    )
  }
}
