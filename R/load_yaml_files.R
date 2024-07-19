#' @title Load yaml files
#'
#' @description This function load yaml files
#'
#' @importFrom yaml read_yaml
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
load_yaml_files <- function() {
  log_debug(x = "Loading default params")
  yaml_files <- c(
    list.files(
      path = file.path(parse_yaml_paths()$params$default),
      pattern = ".yaml",
      full.names = TRUE
    ),
    parse_yaml_paths()$params$prepare_params,
    parse_yaml_paths()$params$prepare_params_advanced
  )

  if (length(list.files(parse_yaml_paths()$params$user$path)) >= length(list.files(parse_yaml_paths()$params$default$path))) {
    yaml_files <- c(
      list.files(
        path = file.path(parse_yaml_paths()$params$user),
        pattern = ".yaml",
        full.names = TRUE
      ),
      parse_yaml_paths()$params$prepare_params,
      parse_yaml_paths()$params$prepare_params_advanced
    )
  }

  yaml_names <- yaml_files |>
    gsub(
      pattern = "inst/params/default/",
      replacement = "",
      fixed = TRUE
    ) |>
    gsub(
      pattern = "inst/params/user/",
      replacement = "",
      fixed = TRUE
    ) |>
    gsub(
      pattern = ".yaml",
      replacement = "",
      fixed = TRUE
    )

  yamls_default <- lapply(X = yaml_files, FUN = read_yaml)

  names(yamls_default) <- yaml_names

  yamls_params <- yamls_default

  return(
    list(
      "yamls_params" = yamls_params,
      "yaml_files" = yaml_files,
      "yaml_names" = yaml_names
    )
  )
}
