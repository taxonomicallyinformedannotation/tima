utils::globalVariables(
  c(
    "arguments",
    "default_path",
    "doc",
    "doc_path",
    "params",
    "user_path"
  )
)

#' @title Get parameters
#'
#' @description This function gets the parameters for the job. Combination of cli and yaml parameters
#'
#' @include parse_cli_params.R
#' @include parse_yaml_params.R
#' @include parse_yaml_paths.R
#'
#' @param step Name of the step being performed
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
get_params <- function(step) {
  paths <- parse_yaml_paths()
  steps <-
    list.files(path = file.path(paths$inst$scripts$docopt)) |>
    stringi::stri_replace_all_fixed(pattern = ".txt", replacement = "")

  stopifnot("Your step does not exist." = step %in% steps)

  doc_path <<-
    file.path(paths$inst$scripts$docopt, paste0(step, ".txt"))
  default_path <<-
    if (step != "prepare_params") {
      file.path(paths$params$default$path, paste0(step, ".yaml"))
    } else {
      file.path(paths$params$prepare_params)
    }

  user_path <<-
    file.path(paths$params$user$path, paste0(step, ".yaml"))

  doc <<- readChar(
    con = doc_path,
    nchars = file.info(doc_path)$size
  )

  arguments <<- docopt::docopt(doc = doc, version = paths$version)

  params <<- parse_yaml_params()

  params <<- parse_cli_params()

  return(params)
}
