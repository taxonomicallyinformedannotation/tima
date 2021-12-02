if (!require(docopt)) {
  install.packages("docopt")
  require(package = "docopt", quietly = TRUE)
}

#' Title
#'
#' @noRd
#'
#' @param step TODO
#'
#' @return TODO
#' @export
#'
#' @examples TODO
get_params <- function(step) {
  doc_path <<- file.path(paths$inst$scripts$docopt, paste0(step, ".txt"))
  default_path <<-
    file.path(paths$config$default$path, paste0(step, ".yaml"))
  params_path <<-
    file.path(paths$config$params$path, paste0(step, ".yaml"))

  doc <<- readChar(
    con = doc_path,
    nchars = file.info(doc_path)$size
  )

  arguments <<- docopt::docopt(doc)

  params <<- parse_yaml_params()

  params <<- parse_cli_params()

  return(params)
}
