import::from(docopt, docopt, .into = environment())
import::from(stringi, stri_replace_all_fixed, .into = environment())

#' @title Get parameters
#'
#' @description This function gets the parameters for the job.
#'    Combination of cli and yaml parameters
#'
#' @importFrom docopt docopt
#' @importFrom stringi stri_replace_all_fixed
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
    list.files(system.file("scripts/docopt", package = "tima")) |>
    stri_replace_all_fixed(pattern = ".txt", replacement = "")

  default_path <-
    if (step == "prepare_params") {
      path_1 <- file.path(paths$params$prepare_params)
      if (file.exists(path_1)) {
        path_1
      } else {
        path_1 |> gsub(pattern = "inst/", replacement = "")
      }
    } else {
      if (step == "prepare_params_advanced") {
        path_2 <- file.path(paths$params$prepare_params_advanced)
        if (file.exists(path_2)) {
          path_2
        } else {
          path_2 |> gsub(pattern = "inst/", replacement = "")
        }
      } else {
        path_3 <- file.path(paths$params$default$path, paste0(step, ".yaml"))
        if (file.exists(path_3)) {
          path_3
        } else {
          path_3 |> gsub(pattern = "inst/", replacement = "")
        }
      }
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
    file.path(system.file("scripts/docopt", package = "tima"), paste0(step, ".txt"))

  usr_path <- if (file.exists(paths$params$user$path)) {
    paths$params$user$path
  } else {
    paths$params$user$path |> gsub(pattern = "inst", replacement = "")
  }
  user_path <-
    file.path(usr_path, paste0(step, ".yaml"))

  doc <- readChar(con = doc_path, nchars = file.info(doc_path)$size)

  params <- parse_yaml_params(def = default_path, usr = user_path)

  params <- parse_cli_params(
    arguments = docopt(doc = doc, version = paths$version),
    parameters = params
  )

  return(params)
}
