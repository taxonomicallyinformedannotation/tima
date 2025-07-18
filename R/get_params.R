#' @title Get parameters
#'
#' @description This function gets the parameters for the job.
#'    Combination of cli and yaml parameters
#'
#' @include get_default_paths.R
#' @include get_path.R
#' @include parse_cli_params.R
#' @include parse_yaml_params.R
#'
#' @param step Name of the step being performed
#'
#' @return The parameters
#'
#' @export
#'
#' @examples
#' \dontrun{
#' copy_backbone()
#' go_to_cache()
#' get_params("prepare_params")
#' }
get_params <- function(step) {
  paths <- get_default_paths()
  steps <-
    list.files(system.file("scripts/docopt", package = "tima")) |>
    stringi::stri_replace_all_fixed(pattern = ".txt", replacement = "")

  default_path <-
    if (step == "prepare_params") {
      file.path(
        system.file(package = "tima"),
        get_path(file.path(paths$params$prepare_params))
      )
    } else if (step == "prepare_params_advanced") {
      file.path(
        system.file(package = "tima"),
        get_path(file.path(paths$params$prepare_params_advanced))
      )
    } else {
      file.path(
        system.file(package = "tima"),
        get_path(file.path(paths$params$default$path, paste0(step, ".yaml")))
      )
    }

  # for advanced parameters to work
  step <- step |>
    gsub(
      pattern = "_advanced",
      replacement = "",
      fixed = TRUE
    )

  stopifnot("Your step does not exist." = step %in% steps)

  doc_path <-
    file.path(
      system.file("scripts/docopt", package = "tima"),
      paste0(step, ".txt")
    )

  user_path <-
    file.path(get_path(paths$params$user$path), paste0(step, ".yaml"))

  doc <- readChar(con = doc_path, nchars = file.info(doc_path)$size)

  if (file.exists(user_path)) {
    params <- parse_yaml_params(def = default_path, usr = user_path)
  } else {
    params <- parse_yaml_params(def = default_path, usr = default_path)
  }

  params <- parse_cli_params(
    arguments = docopt::docopt(doc = doc, version = paths$version),
    parameters = params
  )

  return(params)
}
