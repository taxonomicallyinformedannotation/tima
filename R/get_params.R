#' @title Get parameters
#'
#' @description This function gets the parameters for the job. Combination of cli and yaml parameters
#'
#' @param step Name of the step being performed
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom docopt docopt
#' @importFrom stringr fixed str_remove
#'
#' @examples NULL
get_params <- function(step) {
  paths <- parse_yaml_paths()
  steps <-
    list.files(path = file.path(paths$inst$scripts$docopt)) |>
    stringr::str_remove(pattern = stringr::fixed(".txt"))

  stopifnot("Your step does not exist." = step %in% steps)

  doc_path <<-
    file.path(paths$inst$scripts$docopt, paste0(step, ".txt"))
  default_path <<-
    if (step != "prepare_config") {
      file.path(paths$config$default$path, paste0(step, ".yaml"))
    } else {
      file.path(paths$config$prepare_config)
    }

  user_path <<-
    file.path(paths$config$user$path, paste0(step, ".yaml"))

  doc <<- readChar(
    con = doc_path,
    nchars = file.info(doc_path)$size
  )

  arguments <<- docopt::docopt(doc, version = paths$version)

  params <<- parse_yaml_params()

  params <<- parse_cli_params()

  return(params)
}
