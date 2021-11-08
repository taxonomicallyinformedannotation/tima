#' Title
#'
#' @param step
#'
#' @return
#' @export
#'
#' @examples
get_params <- function(step) {
  doc_path <<- file.path(here::here(paths$inst$scripts$docopt), paste0(step, ".txt"))
  default_path <<-
    file.path(here::here(paths$config$default$path), paste0(step, ".yaml"))
  params_path <<-
    file.path(here::here(paths$config$params$path), paste0(step, ".yaml"))

  doc <<- readChar(
    con = here::here(doc_path),
    nchars = file.info(here::here(doc_path))$size
  )

  arguments <<- docopt(doc)

  params <<- parse_yaml_params()

  params <<- parse_cli_params()

  return(params)
}
