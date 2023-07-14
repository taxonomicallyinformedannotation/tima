utils::globalVariables(c(
  "yaml_default",
  "yaml_files",
  "yaml_names"
))

#' @title Load yaml files
#'
#' @description This function load yaml files

#' @return NULL
#'
#' @export
#'
#' @examples NULL
load_yaml_files <- function() {
  log_debug(x = "Loading default params")
  yaml_files <<- c(
    list.files(
      path = file.path(paths$params$default),
      pattern = ".yaml",
      full.names = TRUE
    ),
    paths$params$prepare_params
  )

  if (length(list.files(paths$params$user$path)) >=
    length(list.files(paths$params$default$path)) -
      ## because of params.yaml
      1) {
    yaml_files <<- c(
      list.files(
        path = file.path(paths$params$user),
        pattern = ".yaml",
        full.names = TRUE
      ),
      paths$params$prepare_params
    )
  }

  yaml_names <<- yaml_files |>
    gsub(pattern = "inst/params/default/", replacement = "") |>
    gsub(pattern = "inst/params/user/", replacement = "") |>
    gsub(pattern = ".yaml", replacement = "")

  yamls_default <<- lapply(
    X = yaml_files,
    FUN = yaml::read_yaml
  )

  names(yamls_default) <<- yaml_names

  yamls_params <<- yamls_default
}
