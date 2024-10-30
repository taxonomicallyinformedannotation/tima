#' @title Get parameters
#'
#' @description This function gets the parameters for the job.
#'    Combination of cli and yaml parameters
#'
#' @include get_default_paths.R
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
#' tima:::copy_backbone()
#' go_to_cache()
#' get_params("prepare_params")
#' }
get_params <- function(step) {
  paths <- get_default_paths()
  steps <-
    list.files(system.file("scripts/docopt", package = "tima")) |>
    stringi::stri_replace_all_fixed(pattern = ".txt", replacement = "")

  get_path <- function(base_path) {
    if (file.exists(base_path)) {
      return(base_path)
    } else {
      new_path <- gsub(pattern = "inst", replacement = "", base_path)
      if (file.exists(new_path)) {
        return(new_path)
      } else {
        return(gsub(
          pattern = "inst",
          replacement = system.file(package = "tima"),
          base_path
        ))
      }
    }
  }

  default_path <-
    if (step == "prepare_params") {
      get_path(file.path(paths$params$prepare_params))
    } else if (step == "prepare_params_advanced") {
      get_path(file.path(paths$params$prepare_params_advanced))
    } else {
      get_path(file.path(paths$params$default$path, paste0(step, ".yaml")))
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

  params <- tima:::parse_yaml_params(def = default_path, usr = user_path)

  params <- tima:::parse_cli_params(
    arguments = docopt::docopt(doc = doc, version = paths$version),
    parameters = params
  )

  return(params)
}
