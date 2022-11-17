#' @title Get parameters
#'
#' @noRd
#'
#' @param step TODO
#'
#' @return TODO
#'
#' @export
#'
#' @importFrom docopt docopt
#'
#' @examples TODO
get_params <- function(step) {
  steps <- list.files(path = file.path(paths$inst$scripts$docopt)) |>
    gsub(pattern = ".txt", replacement = "")

  stopifnot(
    "Your step does not exist." = step %in% steps
  )

  doc_path <<-
    file.path(paths$inst$scripts$docopt, paste0(step, ".txt"))
  default_path <<-
    file.path(paths$config$default$path, paste0(step, ".yaml"))
  params_path <<-
    file.path(paths$config$params$path, paste0(step, ".yaml"))

  doc <<- readChar(
    con = doc_path,
    nchars = file.info(doc_path)$size
  )

  arguments <<- docopt::docopt(doc, version = paths$version)

  params <<- parse_yaml_params()

  params <<- parse_cli_params()

  return(params)
}
